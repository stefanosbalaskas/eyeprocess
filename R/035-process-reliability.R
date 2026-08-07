# Reliability and Generalizability Theory ------------------------------------

.mi_facet_variance <- function(data, metric, facet) {
  values <- aggregate(data[[metric]], by = list(facet = data[[facet]]), FUN = .mi_safe_mean)[[2L]]
  variance <- stats::var(values, na.rm = TRUE)
  if (!is.finite(variance)) 0 else max(0, variance)
}

#' Fit a process-metric generalizability study
#'
#' @param x Long-format process data.
#' @param metric Numeric metric column.
#' @param facets Facet columns, normally person, item, session, and device.
#' @param design Crossed or nested design declaration.
#' @return An `eye_process_gstudy` object.
#' @export
fit_process_gstudy <- function(x, metric, facets = c("person", "item", "session", "device"), design = c("crossed", "nested")) {
  .mi_assert_data(x)
  design <- match.arg(design)
  metric <- as.character(metric)[[1L]]
  facets <- facets[facets %in% names(x)]
  .mi_assert_columns(x, metric)
  if (!length(facets)) .mi_stop("At least one facet column is required.")
  data <- x[is.finite(.mi_numeric(x[[metric]])), c(metric, facets), drop = FALSE]
  data[[metric]] <- .mi_numeric(data[[metric]])
  total_variance <- stats::var(data[[metric]], na.rm = TRUE)
  component <- setNames(vapply(facets, function(facet) .mi_facet_variance(data, metric, facet), numeric(1)), facets)
  if ("person" %in% names(component)) {
    person_name <- "person"
  } else {
    person_name <- facets[[1L]]
  }
  residual <- max(0, total_variance - sum(component))
  components <- data.frame(
    component = c(names(component), "residual"),
    variance = c(component, residual),
    proportion = c(component, residual) / pmax(sum(c(component, residual)), 1e-12),
    stringsAsFactors = FALSE
  )
  counts <- setNames(vapply(facets, function(facet) length(unique(data[[facet]])), integer(1)), facets)
  .mi_new(
    "eye_process_gstudy",
    data = data,
    metric = metric,
    facets = facets,
    person_facet = person_name,
    design = design,
    variance_components = components,
    counts = counts,
    summary = components,
    status = "Generalizability-study variance components estimated with a dependency-free method-of-moments approximation."
  )
}

#' Extract process variance components
#' @param x Generalizability-study object.
#' @return Variance-component table.
#' @export
process_variance_components <- function(x) {
  if (!inherits(x, "eye_process_gstudy")) .mi_stop("`x` must be an `eye_process_gstudy` object.")
  x$variance_components
}

#' Conduct a prospective decision study
#'
#' @param gstudy Generalizability-study object.
#' @param persons Optional person counts retained for reporting.
#' @param items Candidate item counts.
#' @param sessions Candidate session counts.
#' @param devices Candidate device counts.
#' @return An `eye_process_dstudy` object.
#' @export
design_process_dstudy <- function(gstudy, persons = NULL, items = seq(5, 50, 5), sessions = 1:5, devices = 1) {
  if (!inherits(gstudy, "eye_process_gstudy")) .mi_stop("`gstudy` must be an `eye_process_gstudy` object.")
  vc <- setNames(gstudy$variance_components$variance, gstudy$variance_components$component)
  person_var <- vc[[gstudy$person_facet]] %||% 0
  item_var <- vc[["item"]] %||% 0
  session_var <- vc[["session"]] %||% 0
  device_var <- vc[["device"]] %||% 0
  residual_var <- vc[["residual"]] %||% 0
  grid <- expand.grid(items = as.integer(items), sessions = as.integer(sessions), devices = as.integer(devices), KEEP.OUT.ATTRS = FALSE)
  grid$relative_error <- item_var / pmax(grid$items, 1) + session_var / pmax(grid$sessions, 1) + residual_var / pmax(grid$items * grid$sessions * grid$devices, 1)
  grid$absolute_error <- grid$relative_error + device_var / pmax(grid$devices, 1)
  grid$relative_dependability <- person_var / pmax(person_var + grid$relative_error, 1e-12)
  grid$absolute_dependability <- person_var / pmax(person_var + grid$absolute_error, 1e-12)
  .mi_new(
    "eye_process_dstudy",
    gstudy = gstudy,
    design_grid = grid,
    persons = persons,
    summary = grid,
    status = "Prospective D-study dependability coefficients computed."
  )
}

.mi_oneway_icc <- function(values, groups) {
  data <- data.frame(value = .mi_numeric(values), group = factor(groups))
  data <- data[stats::complete.cases(data), ]
  if (nlevels(data$group) < 2L) return(NA_real_)
  means <- aggregate(value ~ group, data = data, FUN = mean)
  counts <- table(data$group)
  grand <- mean(data$value)
  between <- sum(counts[as.character(means$group)] * (means$value - grand)^2) / max(1, nlevels(data$group) - 1L)
  within <- sum((data$value - means$value[match(data$group, means$group)])^2) / max(1, nrow(data) - nlevels(data$group))
  k <- mean(counts)
  (between - within) / pmax(between + (k - 1) * within, 1e-12)
}

#' Audit reliability of multiple process metrics
#'
#' @param x Long-format process data.
#' @param metrics Numeric metric columns.
#' @param method ICC, G-theory, split-half, or bootstrap.
#' @param person_col,item_col Person and item columns.
#' @param draws Bootstrap draws.
#' @return An `eye_process_reliability_audit` object.
#' @export
audit_process_reliability <- function(x, metrics, method = c("icc", "gtheory", "split_half", "bootstrap"), person_col = "person_id", item_col = "item_id", draws = 250) {
  .mi_assert_data(x)
  method <- match.arg(method)
  .mi_assert_columns(x, c(person_col, metrics))
  rows <- lapply(metrics, function(metric) {
    if (method == "icc") {
      estimate <- .mi_oneway_icc(x[[metric]], x[[person_col]])
    } else if (method == "gtheory") {
      facets <- c(person = person_col, item = item_col)
      facets <- unname(facets[facets %in% names(x)])
      gs <- fit_process_gstudy(x, metric, facets = facets)
      ds <- design_process_dstudy(gs, items = length(unique(x[[item_col]] %||% 1)), sessions = 1)
      estimate <- ds$design_grid$relative_dependability[[1L]]
    } else if (method == "split_half") {
      if (!item_col %in% names(x)) .mi_stop("Split-half reliability requires `item_col`.")
      item_levels <- unique(x[[item_col]])
      first <- item_levels[seq_along(item_levels) %% 2L == 1L]
      a <- aggregate(x[[metric]][x[[item_col]] %in% first], by = list(x[[person_col]][x[[item_col]] %in% first]), FUN = .mi_safe_mean)
      b <- aggregate(x[[metric]][!x[[item_col]] %in% first], by = list(x[[person_col]][!x[[item_col]] %in% first]), FUN = .mi_safe_mean)
      merged <- merge(a, b, by = "Group.1")
      correlation <- stats::cor(merged$x.x, merged$x.y, use = "complete.obs")
      estimate <- 2 * correlation / (1 + correlation)
    } else {
      set.seed(20260807)
      person_levels <- unique(x[[person_col]])
      boot <- replicate(draws, {
        selected <- sample(person_levels, length(person_levels), replace = TRUE)
        sampled <- do.call(rbind, lapply(seq_along(selected), function(i) {
          rows <- x[x[[person_col]] == selected[[i]], , drop = FALSE]
          rows[[person_col]] <- paste0("boot_", i)
          rows
        }))
        .mi_oneway_icc(sampled[[metric]], sampled[[person_col]])
      })
      estimate <- .mi_safe_mean(boot)
    }
    data.frame(metric = metric, method = method, estimate = estimate, stringsAsFactors = FALSE)
  })
  summary <- do.call(rbind, rows)
  summary$interpretation <- cut(summary$estimate, breaks = c(-Inf, 0.5, 0.75, 0.9, Inf), labels = c("limited", "moderate", "good", "excellent"), right = FALSE)
  .mi_new(
    "eye_process_reliability_audit",
    data = x,
    summary = summary,
    method = method,
    status = "Process reliability audit completed."
  )
}

#' @export
plot.eye_process_gstudy <- function(x, type = c("variance_components", "diagnostics"), ...) {
  type <- match.arg(type)
  vc <- x$variance_components
  graphics::barplot(vc$variance, names.arg = vc$component, las = 2, ylab = "Variance", main = "Process variance components")
  invisible(x)
}
#' @export
plot.eye_process_dstudy <- function(x, type = c("dependability_surface", "item_sampling"), ...) {
  type <- match.arg(type)
  grid <- x$design_grid
  if (length(unique(grid$sessions)) > 1L) {
    z <- xtabs(absolute_dependability ~ items + sessions, data = grid)
    graphics::image(as.numeric(rownames(z)), as.numeric(colnames(z)), z, xlab = "Items", ylab = "Sessions", main = "Dependability surface")
  } else {
    graphics::plot(grid$items, grid$absolute_dependability, type = "b", ylim = c(0, 1), xlab = "Items", ylab = "Dependability", main = "Item-sampling reliability")
  }
  invisible(x)
}
#' @export
plot.eye_process_reliability_audit <- function(x, type = c("reliability_by_metric", "session_stability"), ...) {
  type <- match.arg(type)
  graphics::barplot(x$summary$estimate, names.arg = x$summary$metric, las = 2, ylim = c(min(0, x$summary$estimate, na.rm = TRUE), 1), ylab = "Reliability", main = "Reliability by process metric")
  graphics::abline(h = c(0.5, 0.75, 0.9), lty = 3)
  invisible(x)
}
#' @export
plot_variance_components <- function(x, ...) plot(x, type = "variance_components", ...)
#' @export
plot_dependability_surface <- function(x, ...) plot(x, type = "dependability_surface", ...)
#' @export
plot_reliability_by_metric <- function(x, ...) plot(x, type = "reliability_by_metric", ...)
#' @export
plot_session_stability <- function(x, ...) plot(x, type = "session_stability", ...)
#' @export
plot_item_sampling_reliability <- function(x, ...) plot(x, type = "item_sampling", ...)
