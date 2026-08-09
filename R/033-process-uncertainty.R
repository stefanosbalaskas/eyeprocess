# Measurement-uncertainty budgets --------------------------------------------

#' Define a process-measurement uncertainty specification
#'
#' @param calibration,aoi_assignment,preprocessing,sampling,model Include sources.
#' @param source_sd Optional named standard deviations by source.
#' @param draws Default simulation draws.
#' @param seed Default seed.
#' @return An `eye_process_uncertainty_spec` object.
#' @export
#' @noRd
process_uncertainty_spec <- function(
    calibration = TRUE,
    aoi_assignment = TRUE,
    preprocessing = TRUE,
    sampling = TRUE,
    model = TRUE,
    source_sd = NULL,
    draws = 1000,
    seed = 20260807) {
  included <- c(
    calibration = isTRUE(calibration),
    aoi_assignment = isTRUE(aoi_assignment),
    preprocessing = isTRUE(preprocessing),
    sampling = isTRUE(sampling),
    model = isTRUE(model)
  )
  if (is.null(source_sd)) source_sd <- setNames(rep(NA_real_, length(included)), names(included))
  source_sd <- .mi_numeric(source_sd)
  names(source_sd) <- names(source_sd) %||% names(included)[seq_along(source_sd)]
  full_sd <- setNames(rep(NA_real_, length(included)), names(included))
  full_sd[names(source_sd)[names(source_sd) %in% names(full_sd)]] <- source_sd[names(source_sd) %in% names(full_sd)]
  out <- list(included = included, source_sd = full_sd, draws = as.integer(draws), seed = as.integer(seed))
  class(out) <- "eye_process_uncertainty_spec"
  out
}

#' @export
print.eye_process_uncertainty_spec <- function(x, ...) {
  cat("eyeprocess uncertainty specification\n")
  print(data.frame(source = names(x$included), included = unname(x$included), source_sd = unname(x$source_sd)), row.names = FALSE)
  invisible(x)
}

#' Estimate uncertainty components for process metrics
#'
#' @param x Numeric vector or data frame.
#' @param spec Uncertainty specification.
#' @param metrics Numeric columns to analyse.
#' @param cluster Optional cluster column for cluster-robust sampling uncertainty.
#' @return An `eye_process_uncertainty` object.
#' @export
#' @noRd
estimate_process_uncertainty <- function(x, spec = process_uncertainty_spec(), metrics = NULL, cluster = NULL) {
  if (!inherits(spec, "eye_process_uncertainty_spec")) .mi_stop("`spec` must be created by process_uncertainty_spec().")
  if (is.numeric(x) && is.null(dim(x))) x <- data.frame(metric = x)
  .mi_assert_data(x)
  metrics <- metrics %||% names(x)[vapply(x, is.numeric, logical(1))]
  metrics <- intersect(metrics, names(x))
  if (!length(metrics)) .mi_stop("No numeric process metrics were selected.")
  rows <- list()
  k <- 1L
  for (metric in metrics) {
    values <- .mi_numeric(x[[metric]])
    values <- values[is.finite(values)]
    n <- length(values)
    estimate <- .mi_safe_mean(values)
    raw_sd <- .mi_safe_sd(values)
    sampling_se <- if (!is.null(cluster) && cluster %in% names(x)) {
      cluster_means <- aggregate(x[[metric]], by = list(x[[cluster]]), FUN = .mi_safe_mean)[[2L]]
      .mi_safe_sd(cluster_means) / sqrt(max(1, length(cluster_means)))
    } else raw_sd / sqrt(max(1, n))
    default_sd <- c(
      calibration = 0.08,
      aoi_assignment = 0.10,
      preprocessing = 0.07,
      sampling = 1.00,
      model = 0.05
    ) * if (is.finite(raw_sd)) raw_sd else 1
    source_sd <- spec$source_sd
    missing_sd <- !is.finite(source_sd)
    source_sd[missing_sd] <- default_sd[names(source_sd)[missing_sd]]
    source_sd["sampling"] <- sampling_se
    source_sd[!spec$included] <- 0
    source_variance <- source_sd^2
    total_se <- sqrt(sum(source_variance, na.rm = TRUE))
    for (source in names(source_variance)) {
      rows[[k]] <- data.frame(
        metric = metric,
        source = source,
        estimate = estimate,
        n = n,
        source_sd = source_sd[[source]],
        source_variance = source_variance[[source]],
        variance_share = if (sum(source_variance) > 0) source_variance[[source]] / sum(source_variance) else NA_real_,
        total_se = total_se,
        lower = estimate - 1.96 * total_se,
        upper = estimate + 1.96 * total_se,
        stringsAsFactors = FALSE
      )
      k <- k + 1L
    }
  }
  components <- do.call(rbind, rows)
  summary <- unique(components[, c("metric", "estimate", "n", "total_se", "lower", "upper")])
  .mi_new(
    "eye_process_uncertainty",
    components = components,
    summary = summary,
    spec = spec,
    data = x,
    metrics = metrics,
    status = "Measurement-uncertainty components estimated."
  )
}

#' Propagate process uncertainty to an estimand
#'
#' @param x Data frame, numeric vector, or process-uncertainty object.
#' @param estimand Function accepting a resampled data object.
#' @param method Bootstrap, simulation, or posterior propagation.
#' @param draws Number of draws.
#' @param seed Random seed.
#' @return An `eye_process_uncertainty_propagation` object.
#' @export
#' @noRd
propagate_process_uncertainty <- function(
    x,
    estimand = function(data) mean(data, na.rm = TRUE),
    method = c("bootstrap", "simulation", "posterior"),
    draws = NULL,
    seed = NULL) {
  method <- match.arg(method)
  uncertainty <- if (inherits(x, "eye_process_uncertainty")) x else NULL
  data <- if (!is.null(uncertainty)) uncertainty$data else x
  if (!is.function(estimand)) .mi_stop("`estimand` must be a function.")
  draws <- as.integer(draws %||% if (!is.null(uncertainty)) uncertainty$spec$draws else 1000L)
  seed <- as.integer(seed %||% if (!is.null(uncertainty)) uncertainty$spec$seed else 20260807L)
  set.seed(seed)
  values <- numeric(draws)
  if (method == "posterior") {
    posterior <- if (is.list(data) && !is.null(data$draws)) data$draws else data
    posterior <- .mi_numeric(posterior)
    posterior <- posterior[is.finite(posterior)]
    if (!length(posterior)) .mi_stop("Posterior propagation requires numeric draws.")
    values <- sample(posterior, draws, replace = TRUE)
  } else {
    n <- if (is.data.frame(data)) nrow(data) else length(data)
    if (!n) .mi_stop("No observations are available for uncertainty propagation.")
    for (i in seq_len(draws)) {
      index <- sample.int(n, n, replace = TRUE)
      sampled <- if (is.data.frame(data)) data[index, , drop = FALSE] else data[index]
      if (method == "simulation" && !is.null(uncertainty)) {
        total_sd <- mean(uncertainty$summary$total_se, na.rm = TRUE)
        if (is.data.frame(sampled)) {
          numeric_cols <- vapply(sampled, is.numeric, logical(1))
          sampled[numeric_cols] <- lapply(sampled[numeric_cols], function(column) column + stats::rnorm(length(column), 0, total_sd))
        } else sampled <- sampled + stats::rnorm(length(sampled), 0, total_sd)
      }
      values[[i]] <- tryCatch(as.numeric(estimand(sampled))[[1L]], error = function(e) NA_real_)
    }
  }
  values <- values[is.finite(values)]
  summary <- data.frame(
    method = method,
    draws = length(values),
    mean = .mi_safe_mean(values),
    sd = .mi_safe_sd(values),
    lower = .mi_safe_quantile(values, 0.025),
    median = .mi_safe_quantile(values, 0.5),
    upper = .mi_safe_quantile(values, 0.975),
    stringsAsFactors = FALSE
  )
  .mi_new(
    "eye_process_uncertainty_propagation",
    draws = values,
    summary = summary,
    method = method,
    source = x,
    status = "Process uncertainty propagated to the requested estimand."
  )
}

#' Return a normalized uncertainty budget
#'
#' @param x Process-uncertainty object.
#' @return Source-by-metric budget table.
#' @export
#' @noRd
uncertainty_budget <- function(x) {
  if (!inherits(x, "eye_process_uncertainty")) .mi_stop("`x` must be an `eye_process_uncertainty` object.")
  out <- x$components[, c("metric", "source", "source_sd", "source_variance", "variance_share")]
  rownames(out) <- NULL
  out
}

#' Compare multiple uncertainty budgets
#'
#' @param ... Process-uncertainty objects.
#' @return An `eye_uncertainty_budget_comparison` object.
#' @export
#' @noRd
compare_uncertainty_budgets <- function(...) {
  objects <- list(...)
  if (!length(objects)) .mi_stop("Supply at least one uncertainty object.")
  labels <- names(objects)
  if (is.null(labels) || any(!nzchar(labels))) labels <- paste0("budget_", seq_along(objects))
  tables <- lapply(seq_along(objects), function(i) {
    if (!inherits(objects[[i]], "eye_process_uncertainty")) .mi_stop("All objects must be process-uncertainty results.")
    cbind(budget = labels[[i]], uncertainty_budget(objects[[i]]), stringsAsFactors = FALSE)
  })
  combined <- do.call(rbind, tables)
  .mi_new(
    "eye_uncertainty_budget_comparison",
    budgets = objects,
    combined = combined,
    summary = aggregate(variance_share ~ budget + source, data = combined, FUN = .mi_safe_mean),
    status = "Uncertainty budgets compared."
  )
}

#' @export
plot.eye_process_uncertainty <- function(x, type = c("waterfall", "tornado", "by_item", "by_stage", "diagnostics"), metric = NULL, ...) {
  type <- match.arg(type)
  components <- x$components
  metric <- metric %||% unique(components$metric)[[1L]]
  selected <- components[components$metric == metric, , drop = FALSE]
  if (!nrow(selected)) return(.mi_plot_empty("Selected metric is unavailable."))
  if (type %in% c("waterfall", "by_stage", "diagnostics")) {
    graphics::barplot(selected$source_variance, names.arg = selected$source, las = 2,
                      ylab = "Variance contribution", main = paste("Uncertainty budget:", metric))
  } else if (type == "tornado") {
    order <- order(selected$source_sd)
    graphics::barplot(selected$source_sd[order], names.arg = selected$source[order], horiz = TRUE, las = 1,
                      xlab = "Source standard deviation", main = paste("Uncertainty tornado:", metric))
  } else {
    metrics <- unique(components$metric)
    matrix <- sapply(metrics, function(one) components$variance_share[components$metric == one])
    rownames(matrix) <- unique(components$source)
    graphics::barplot(matrix, beside = FALSE, names.arg = metrics, las = 2,
                      ylab = "Variance share", main = "Uncertainty by metric")
  }
  invisible(x)
}

#' @export
plot.eye_process_uncertainty_propagation <- function(x, type = c("distribution", "sensitivity"), ...) {
  type <- match.arg(type)
  graphics::hist(x$draws, xlab = "Estimand", main = if (type == "sensitivity") "Uncertainty sensitivity" else "Propagated estimand uncertainty")
  graphics::abline(v = x$summary$median, lty = 2, lwd = 2)
  invisible(x)
}

#' @export
plot.eye_uncertainty_budget_comparison <- function(x, type = c("comparison", "by_stage"), ...) {
  type <- match.arg(type)
  table <- x$summary
  budgets <- unique(table$budget)
  sources <- unique(table$source)
  matrix <- matrix(0, nrow = length(sources), ncol = length(budgets), dimnames = list(sources, budgets))
  for (i in seq_len(nrow(table))) matrix[table$source[[i]], table$budget[[i]]] <- table$variance_share[[i]]
  graphics::barplot(matrix, beside = type == "comparison", las = 2, ylab = "Mean variance share", main = "Uncertainty-budget comparison")
  graphics::legend("topright", legend = rownames(matrix), fill = seq_len(nrow(matrix)), bty = "n")
  invisible(x)
}

#' @export
plot_uncertainty_waterfall <- function(x, ...) plot(x, type = "waterfall", ...)
#' @export
plot_uncertainty_tornado <- function(x, ...) plot(x, type = "tornado", ...)
#' @export
plot_uncertainty_by_item <- function(x, ...) plot(x, type = "by_item", ...)
#' @export
plot_uncertainty_by_stage <- function(x, ...) plot(x, type = "by_stage", ...)
