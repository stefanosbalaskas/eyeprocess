# Informative missingness and MNAR sensitivity --------------------------------

#' Fit a process-signal observation model
#'
#' @param x Process data.
#' @param observed Observation indicator column or logical vector.
#' @param predictors Predictor column names.
#' @param random Random-effect grouping columns recorded for audit.
#' @return An `eye_process_observation_model` object.
#' @export
fit_process_observation_model <- function(x, observed, predictors, random = c("person", "item")) {
  .mi_assert_data(x)
  observed_values <- if (length(observed) == 1L && is.character(observed) && observed %in% names(x)) x[[observed]] else observed
  if (length(observed_values) != nrow(x)) .mi_stop("`observed` must contain one value per row.")
  predictors <- predictors[predictors %in% names(x)]
  if (!length(predictors)) .mi_stop("At least one observation predictor is required.")
  data <- x[, predictors, drop = FALSE]
  data$.observed <- as.integer(as.logical(observed_values))
  formula <- .mi_formula(".observed", predictors)
  model <- stats::glm(formula, data = data, family = stats::binomial())
  probability <- as.numeric(stats::predict(model, type = "response"))
  .mi_new(
    "eye_process_observation_model",
    model = model,
    data = data,
    probability = probability,
    random = random,
    summary = as.data.frame(summary(model)$coefficients),
    status = "Process-signal observation model fitted. Random group declarations are audit metadata in the dependency-free engine."
  )
}

#' Fit a joint outcome-missingness approximation
#'
#' @param outcome Numeric outcome vector or column name.
#' @param observation Observation-model object or indicator.
#' @param method Selection or shared-parameter approximation.
#' @param x Optional data frame when column names are supplied.
#' @param predictors Additional outcome predictors.
#' @return An `eye_joint_signal_missingness` object.
#' @export
fit_joint_signal_missingness <- function(outcome, observation, method = c("selection", "shared_parameter"), x = NULL, predictors = NULL) {
  method <- match.arg(method)
  if (!is.null(x)) .mi_assert_data(x)
  y <- if (is.character(outcome) && length(outcome) == 1L && !is.null(x)) .mi_numeric(x[[outcome]]) else .mi_numeric(outcome)
  observed <- if (inherits(observation, "eye_process_observation_model")) observation$data$.observed else if (is.character(observation) && !is.null(x)) as.integer(as.logical(x[[observation]])) else as.integer(as.logical(observation))
  if (length(y) != length(observed)) .mi_stop("Outcome and observation indicators must have equal length.")
  data <- data.frame(outcome = y, observed = observed)
  if (!is.null(x) && length(predictors)) data <- cbind(data, x[, predictors[predictors %in% names(x)], drop = FALSE])
  formula <- .mi_formula("outcome", c("observed", intersect(predictors, names(data))))
  model <- stats::lm(formula, data = data, subset = is.finite(outcome))
  shared_correlation <- if (inherits(observation, "eye_process_observation_model")) {
    stats::cor(stats::residuals(model), observation$probability[is.finite(y)], use = "complete.obs")
  } else NA_real_
  .mi_new(
    "eye_joint_signal_missingness",
    model = model,
    data = data,
    method = method,
    shared_correlation = shared_correlation,
    summary = as.data.frame(summary(model)$coefficients),
    status = "Joint signal-missingness approximation fitted; use a full shared-parameter model for confirmatory MNAR inference."
  )
}

#' Run a pattern-mixture delta sensitivity analysis
#'
#' @param x Numeric vector or data frame.
#' @param delta Delta adjustment grid.
#' @param metric Metric column for data frames.
#' @param estimand Function applied after imputation.
#' @return An `eye_mnar_sensitivity` object.
#' @export
process_pattern_mixture <- function(x, delta = seq(-1, 1, 0.1), metric = NULL, estimand = mean) {
  values <- if (is.data.frame(x)) {
    metric <- metric %||% names(x)[vapply(x, is.numeric, logical(1))][[1L]]
    .mi_numeric(x[[metric]])
  } else .mi_numeric(x)
  observed <- values[is.finite(values)]
  missing <- !is.finite(values)
  if (!length(observed)) .mi_stop("At least one observed value is required.")
  center <- mean(observed); spread <- stats::sd(observed)
  if (!is.finite(spread)) spread <- 0
  rows <- lapply(delta, function(one_delta) {
    completed <- values
    completed[missing] <- center + one_delta * spread
    data.frame(delta = one_delta, estimate = as.numeric(estimand(completed, na.rm = TRUE))[[1L]], imputed_value = center + one_delta * spread, missing_fraction = mean(missing))
  })
  table <- do.call(rbind, rows)
  .mi_new("eye_mnar_sensitivity", table = table, summary = table, metric = metric, status = "Pattern-mixture MNAR sensitivity completed.")
}

#' Summarize MNAR tipping points
#'
#' @param x MNAR sensitivity object or raw values.
#' @param estimand Optional estimand for raw values.
#' @param null Null effect value.
#' @param ... Passed to `process_pattern_mixture()` for raw values.
#' @return An `eye_mnar_tipping_point` object.
#' @export
sensitivity_mnar_process <- function(x, estimand = mean, null = 0, ...) {
  sensitivity <- if (inherits(x, "eye_mnar_sensitivity")) x else process_pattern_mixture(x, estimand = estimand, ...)
  signs <- sign(sensitivity$table$estimate - null)
  changes <- which(diff(signs) != 0)
  tipping <- if (length(changes)) mean(sensitivity$table$delta[changes[[1L]] + c(0, 1)]) else NA_real_
  .mi_new(
    "eye_mnar_tipping_point",
    sensitivity = sensitivity,
    tipping_delta = tipping,
    summary = data.frame(tipping_delta = tipping, stable_over_grid = !is.finite(tipping)),
    status = if (is.finite(tipping)) "An MNAR tipping point was detected." else "The estimand did not cross the null over the tested delta grid."
  )
}

#' @export
plot.eye_process_observation_model <- function(x, type = c("observation_probability", "missingness_time", "missingness_aoi", "diagnostics"), time = NULL, aoi = NULL, ...) {
  type <- match.arg(type)
  if (type == "observation_probability" || type == "diagnostics") {
    graphics::hist(x$probability, xlab = "Predicted observation probability", main = "Process-signal observation model")
  } else if (type == "missingness_time" && !is.null(time)) {
    graphics::plot(time, x$probability, xlab = "Time", ylab = "Observation probability", main = "Missingness over time")
  } else if (type == "missingness_aoi" && !is.null(aoi)) {
    means <- aggregate(x$probability, by = list(aoi = aoi), FUN = .mi_safe_mean)
    graphics::barplot(means$x, names.arg = means$aoi, las = 2, ylab = "Observation probability", main = "Missingness by AOI")
  } else .mi_plot_empty("Supply the requested time or AOI vector.")
  invisible(x)
}
#' @export
plot.eye_mnar_sensitivity <- function(x, type = c("tipping_point", "complete_case_sensitivity", "sensitivity"), ...) {
  type <- match.arg(type)
  graphics::plot(x$table$delta, x$table$estimate, type = "b", xlab = "MNAR delta", ylab = "Estimand", main = "MNAR sensitivity")
  graphics::abline(h = 0, lty = 2)
  invisible(x)
}
#' @export
plot.eye_mnar_tipping_point <- function(x, type = c("tipping_point", "sensitivity"), ...) {
  plot(x$sensitivity, type = "tipping_point", ...)
  if (is.finite(x$tipping_delta)) graphics::abline(v = x$tipping_delta, lty = 2, lwd = 2)
  invisible(x)
}
#' @export
plot_observation_probability <- function(x, ...) plot(x, type = "observation_probability", ...)
#' @export
plot_missingness_by_time <- function(x, ...) plot(x, type = "missingness_time", ...)
#' @export
plot_missingness_by_aoi <- function(x, ...) plot(x, type = "missingness_aoi", ...)
#' @export
plot_mnar_tipping_point <- function(x, ...) plot(x, type = "tipping_point", ...)
#' @export
plot_complete_case_sensitivity <- function(x, ...) plot(x, type = "complete_case_sensitivity", ...)
