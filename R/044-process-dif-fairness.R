# Dynamic process-DIF and fairness drift --------------------------------------

#' Fit item-level psychometric and process DIF models
#'
#' @param x Person-item data.
#' @param response Response column.
#' @param process Process-metric column.
#' @param group Group column.
#' @param item Item column.
#' @param ability Optional ability covariate.
#' @return An `eye_process_dif` object.
#' @export
#' @noRd
fit_process_dif <- function(x, response, process, group, item, ability = NULL) {
  .mi_assert_data(x)
  columns <- c(response, process, group, item, ability)
  columns <- columns[!is.null(columns) & nzchar(columns)]
  .mi_assert_columns(x, columns)
  items <- unique(as.character(x[[item]]))
  rows <- vector("list", length(items)); models <- vector("list", length(items)); names(models) <- items
  for (i in seq_along(items)) {
    data <- x[as.character(x[[item]]) == items[[i]], , drop = FALSE]
    data$.response <- .mi_numeric(data[[response]]); data$.process <- .mi_z(data[[process]]); data$.group <- factor(data[[group]])
    if (!is.null(ability)) data$.ability <- .mi_numeric(data[[ability]])
    predictors <- c(".group", ".process", ".group:.process", if (!is.null(ability)) ".ability")
    formula <- stats::as.formula(paste(".response ~", paste(predictors, collapse = " + ")))
    binary <- all(stats::na.omit(unique(data$.response)) %in% c(0, 1))
    model <- if (binary) stats::glm(formula, data = data, family = stats::binomial()) else stats::lm(formula, data = data)
    models[[i]] <- model
    coefficients <- as.data.frame(summary(model)$coefficients)
    coefficients$term <- rownames(coefficients); rownames(coefficients) <- NULL
    group_rows <- grepl("^.group", coefficients$term)
    process_interaction <- grepl(".group.*:.process|.process:.group", coefficients$term)
    rows[[i]] <- data.frame(
      item_id = items[[i]],
      n = nrow(data),
      psychometric_dif = if (any(group_rows & !process_interaction)) max(abs(coefficients[group_rows & !process_interaction, 1L])) else NA_real_,
      process_dif = if (any(process_interaction)) max(abs(coefficients[process_interaction, 1L])) else NA_real_,
      psychometric_p = if (any(group_rows & !process_interaction)) min(coefficients[group_rows & !process_interaction, 4L]) else NA_real_,
      process_p = if (any(process_interaction)) min(coefficients[process_interaction, 4L]) else NA_real_,
      stringsAsFactors = FALSE
    )
  }
  summary <- do.call(rbind, rows)
  summary$review_flag <- (summary$psychometric_p < 0.05) | (summary$process_p < 0.05)
  .mi_new("eye_process_dif", models = models, summary = summary, data = x, response = response, process = process, group = group, item = item, status = "Psychometric and process DIF models fitted item by item.")
}

#' Monitor DIF drift over time or deployment batches
#'
#' @param x Person-item-time data.
#' @param time Time or batch column.
#' @param group Group column.
#' @param metrics Metrics to compare.
#' @param item Optional item column.
#' @return An `eye_dif_drift` object.
#' @export
#' @noRd
monitor_dif_drift <- function(x, time, group, metrics, item = NULL) {
  .mi_assert_data(x)
  columns <- c(time, group, metrics, item); columns <- columns[!is.null(columns)]
  .mi_assert_columns(x, columns)
  group_cols <- c(time, group, item); group_cols <- group_cols[!is.null(group_cols)]
  rows <- list(); k <- 1L
  for (metric in metrics) {
    formula <- stats::as.formula(paste(metric, "~", paste(group_cols, collapse = " + ")))
    means <- aggregate(formula, data = x, FUN = .mi_safe_mean)
    if (is.null(item)) means$item_id <- "all_items" else names(means)[names(means) == item] <- "item_id"
    names(means)[names(means) == metric] <- "mean_value"
    means$metric <- metric
    rows[[k]] <- means; k <- k + 1L
  }
  trajectories <- do.call(rbind, rows)
  split_key <- interaction(trajectories$metric, trajectories$item_id, trajectories[[group]], drop = TRUE)
  slopes <- do.call(rbind, lapply(split(trajectories, split_key), function(data) {
    time_value <- .mi_numeric(data[[time]])
    mean_value <- .mi_numeric(data$mean_value)
    keep <- is.finite(time_value) & is.finite(mean_value)
    time_value <- time_value[keep]
    mean_value <- mean_value[keep]
    slope <- NA_real_
    if (length(time_value) >= 2L && length(unique(time_value)) >= 2L) {
      model <- stats::lm(mean_value ~ time_value)
      coefficient <- stats::coef(model)
      if (length(coefficient) >= 2L && is.finite(coefficient[[2L]])) {
        slope <- unname(coefficient[[2L]])
      }
    }
    data.frame(
      metric = data$metric[[1L]],
      item_id = data$item_id[[1L]],
      group = as.character(data[[group]][[1L]]),
      slope = slope,
      n_time_points = length(unique(time_value)),
      stringsAsFactors = FALSE
    )
  }))
  .mi_new("eye_dif_drift", trajectories = trajectories, slopes = slopes, time = time, group = group, summary = slopes, status = "Process and fairness drift trajectories summarized.")
}

#' Decompose psychometric, process, and design-feature DIF evidence
#'
#' @param psychometric Psychometric DIF table or process-DIF object.
#' @param process Process-DIF table.
#' @param design_features Item design-feature table.
#' @return An `eye_dif_decomposition` object.
#' @export
#' @noRd
decompose_dif_evidence <- function(psychometric, process = NULL, design_features = NULL) {
  psychometric_table <- if (inherits(psychometric, "eye_process_dif")) psychometric$summary else as.data.frame(psychometric)
  if (!"item_id" %in% names(psychometric_table)) .mi_stop("Psychometric evidence requires `item_id`.")
  table <- psychometric_table
  if (!is.null(process)) table <- merge(table, as.data.frame(process), by = "item_id", all = TRUE, suffixes = c("_psychometric", "_process"))
  if (!is.null(design_features)) table <- merge(table, as.data.frame(design_features), by = "item_id", all = TRUE)
  psych_flag <- if ("psychometric_p" %in% names(table)) table$psychometric_p < 0.05 else table$review_flag %||% FALSE
  process_flag <- if ("process_p" %in% names(table)) table$process_p < 0.05 else FALSE
  table$evidence_pattern <- ifelse(psych_flag & process_flag, "convergent_psychometric_and_process_difference", ifelse(psych_flag, "psychometric_only", ifelse(process_flag, "process_only", "no_flag")))
  .mi_new("eye_dif_decomposition", table = table, summary = table, status = "DIF evidence decomposed. Interpret associations without causal language unless the design supports it.")
}

#' Audit fairness transportability
#'
#' @param x DIF summary data.
#' @param context Context column such as site, device, or session.
#' @param effect Effect column.
#' @param item Item column.
#' @return An `eye_fairness_transportability` object.
#' @export
#' @noRd
audit_fairness_transportability <- function(x, context = "device", effect = "process_dif", item = "item_id") {
  data <- if (inherits(x, "eye_process_dif")) x$summary else as.data.frame(x)
  .mi_assert_columns(data, c(item, effect))
  if (!context %in% names(data)) data[[context]] <- "single_context"
  matrix <- stats::reshape(data[, c(item, context, effect)], idvar = item, timevar = context, direction = "wide")
  effect_cols <- setdiff(names(matrix), item)
  correlations <- if (length(effect_cols) >= 2L) stats::cor(matrix[, effect_cols, drop = FALSE], use = "pairwise.complete.obs") else matrix(1, 1, 1, dimnames = list(effect_cols, effect_cols))
  .mi_new("eye_fairness_transportability", data = data, effect_matrix = matrix, correlation = correlations,
          summary = data.frame(contexts = length(effect_cols), mean_cross_context_correlation = if (length(correlations) > 1L) mean(correlations[upper.tri(correlations)], na.rm = TRUE) else 1), status = "Fairness transportability audited across available contexts.")
}

#' @export
plot.eye_process_dif <- function(x, type = c("forest", "group_curves", "icc_overlay", "diagnostics"), ...) {
  type <- match.arg(type)
  summary <- x$summary; y <- seq_len(nrow(summary))
  effect <- if (type == "forest") summary$process_dif else summary$psychometric_dif
  graphics::plot(effect, y, yaxt = "n", xlab = "Absolute DIF effect", ylab = "", main = "Item process-DIF evidence")
  graphics::axis(2, at = y, labels = summary$item_id, las = 2); graphics::abline(v = 0, lty = 2)
  invisible(x)
}
#' @export
plot.eye_dif_drift <- function(x, type = c("heatmap", "curves", "diagnostics"), metric = NULL, ...) {
  type <- match.arg(type); metric <- metric %||% unique(x$trajectories$metric)[[1L]]
  data <- x$trajectories[x$trajectories$metric == metric, ]
  if (type == "heatmap") {
    table <- xtabs(mean_value ~ item_id + .mi_numeric(data[[x$time]]), data = data)
    graphics::image(as.numeric(seq_len(nrow(table))), as.numeric(seq_len(ncol(table))), table, xlab = "Item", ylab = "Time", main = "DIF drift heatmap")
  } else {
    groups <- unique(data[[x$group]])
    graphics::plot(range(.mi_numeric(data[[x$time]])), range(data$mean_value), type = "n", xlab = x$time, ylab = metric, main = "Group process curves over time")
    for (i in seq_along(groups)) {
      one <- data[data[[x$group]] == groups[[i]], ]; graphics::lines(.mi_numeric(one[[x$time]]), one$mean_value, type = "b", lty = i)
    }
    graphics::legend("topright", legend = groups, lty = seq_along(groups), bty = "n")
  }
  invisible(x)
}
#' @export
plot.eye_fairness_transportability <- function(x, type = c("transport_matrix", "diagnostics"), ...) {
  graphics::image(t(x$correlation[nrow(x$correlation):1L, , drop = FALSE]), axes = FALSE, main = "Fairness transportability matrix")
  invisible(x)
}
#' @export
plot_group_icc_process_overlay <- function(x, ...) plot(x, type = "icc_overlay", ...)
#' @export
plot_process_dif_forest <- function(x, ...) plot(x, type = "forest", ...)
#' @export
plot_dif_drift_heatmap <- function(x, ...) plot(x, type = "heatmap", ...)
#' @export
plot_fairness_transport_matrix <- function(x, ...) plot(x, type = "transport_matrix", ...)
#' @export
plot_item_group_process_curves <- function(x, ...) plot(x, type = "group_curves", ...)
