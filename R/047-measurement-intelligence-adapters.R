# Compatibility aliases and cross-modal recurrence model ---------------------

#' Fit a process missingness model
#'
#' This is a compatibility alias for `fit_process_observation_model()`.
#'
#' @inheritParams fit_process_observation_model
#' @return An `eye_process_observation_model` object.
#' @export
fit_process_missingness_model <- function(x, observed, predictors, random = c("person", "item")) {
  fit_process_observation_model(x, observed = observed, predictors = predictors, random = random)
}

#' Fit a cross-modal recurrence outcome model
#'
#' @param x,y Synchronized numeric signals.
#' @param outcome Optional outcome vector. When omitted, the function returns a
#'   recurrence-feature model object without a downstream regression.
#' @param channels Channel declaration passed to `cross_recurrence()`.
#' @param radius Optional recurrence radius.
#' @param covariates Optional covariate data frame with one row per observation
#'   in `outcome`.
#' @return An `eye_crossmodal_recurrence_model` object.
#' @export
crossmodal_recurrence_model <- function(
    x,
    y,
    outcome = NULL,
    channels = c("gaze_pupil", "gaze_eda", "pupil_eda"),
    radius = NULL,
    covariates = NULL) {
  channels <- match.arg(channels)
  recurrence <- cross_recurrence(x, y, channels = channels, radius = radius)
  features <- recurrence_features(recurrence)
  model <- NULL
  model_data <- NULL
  if (!is.null(outcome)) {
    outcome <- .mi_numeric(outcome)
    if (is.null(covariates)) {
      model_data <- data.frame(outcome = outcome)
    } else {
      .mi_assert_data(covariates)
      if (nrow(covariates) != length(outcome)) .mi_stop("`covariates` must align with `outcome`.")
      model_data <- cbind(data.frame(outcome = outcome), covariates)
    }
    for (name in names(features)) model_data[[name]] <- features[[name]]
    predictors <- setdiff(names(model_data)[vapply(model_data, is.numeric, logical(1))], "outcome")
    model <- stats::lm(.mi_formula("outcome", predictors), data = model_data)
  }
  summary <- if (is.null(model)) features else as.data.frame(summary(model)$coefficients)
  .mi_new(
    "eye_crossmodal_recurrence_model",
    recurrence = recurrence,
    features = features,
    model = model,
    data = model_data,
    summary = summary,
    status = if (is.null(model)) "Cross-modal recurrence features calculated." else "Cross-modal recurrence outcome model fitted."
  )
}

#' @export
plot.eye_crossmodal_recurrence_model <- function(x, type = c("crossmodal", "effects", "diagnostics"), ...) {
  type <- match.arg(type)
  if (type == "crossmodal") {
    plot(x$recurrence, type = "crossmodal", ...)
  } else if (is.null(x$model)) {
    .mi_plot_empty("No downstream outcome model was fitted.")
  } else if (type == "diagnostics") {
    graphics::plot(stats::fitted(x$model), stats::residuals(x$model), xlab = "Fitted", ylab = "Residual", main = "Cross-modal recurrence model diagnostics")
    graphics::abline(h = 0, lty = 2)
  } else {
    graphics::barplot(stats::coef(x$model), las = 2, ylab = "Coefficient", main = "Cross-modal recurrence effects")
  }
  invisible(x)
}
