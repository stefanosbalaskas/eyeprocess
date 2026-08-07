# Calibration drift and offline recalibration --------------------------------

.mi_reference_table <- function(x, references, x_col, y_col) {
  if (is.null(references)) {
    tx <- .mi_first_column(x, c("target_x", "reference_x", "expected_x"), label = "reference x coordinate")
    ty <- .mi_first_column(x, c("target_y", "reference_y", "expected_y"), label = "reference y coordinate")
    return(data.frame(observed_x = .mi_numeric(x[[x_col]]), observed_y = .mi_numeric(x[[y_col]]),
                      reference_x = .mi_numeric(x[[tx]]), reference_y = .mi_numeric(x[[ty]])))
  }
  .mi_assert_data(references)
  if (nrow(references) == nrow(x)) {
    rx <- .mi_first_column(references, c("reference_x", "target_x", "x"), label = "reference x coordinate")
    ry <- .mi_first_column(references, c("reference_y", "target_y", "y"), label = "reference y coordinate")
    return(data.frame(observed_x = .mi_numeric(x[[x_col]]), observed_y = .mi_numeric(x[[y_col]]),
                      reference_x = .mi_numeric(references[[rx]]), reference_y = .mi_numeric(references[[ry]])))
  }
  key <- intersect(c("target_id", "reference_id", "marker_id", "aoi_id"), intersect(names(x), names(references)))
  if (!length(key)) .mi_stop("`references` must have one row per observation or share a target identifier with `x`.")
  key <- key[[1L]]
  rx <- .mi_first_column(references, c("reference_x", "target_x", "x"), label = "reference x coordinate")
  ry <- .mi_first_column(references, c("reference_y", "target_y", "y"), label = "reference y coordinate")
  merged <- merge(x, references[, c(key, rx, ry)], by = key, all.x = TRUE, sort = FALSE)
  data.frame(observed_x = .mi_numeric(merged[[x_col]]), observed_y = .mi_numeric(merged[[y_col]]),
             reference_x = .mi_numeric(merged[[rx]]), reference_y = .mi_numeric(merged[[ry]]))
}

#' Detect spatial calibration drift
#'
#' @param x Gaze observations with reference coordinates.
#' @param references Optional reference table.
#' @param window Window size in time units or text such as `"30 sec"`.
#' @param method Targets, known AOIs, or fixation-density heuristic.
#' @param x_col,y_col,time_col Coordinate and time columns.
#' @return An `eye_calibration_drift` object.
#' @export
detect_calibration_drift <- function(
    x,
    references = NULL,
    window = "30 sec",
    method = c("targets", "known_aois", "fixation_density"),
    x_col = NULL,
    y_col = NULL,
    time_col = NULL) {
  .mi_assert_data(x)
  method <- match.arg(method)
  x_col <- x_col %||% .mi_first_column(x, c("x", "gaze_x", "x_norm", "x_px"), label = "gaze x coordinate")
  y_col <- y_col %||% .mi_first_column(x, c("y", "gaze_y", "y_norm", "y_px"), label = "gaze y coordinate")
  time_col <- time_col %||% .mi_first_column(x, c("time", "timestamp", "time_sec", "sample_time"), required = FALSE)
  reference <- .mi_reference_table(x, references, x_col, y_col)
  time <- if (!is.na(time_col)) .mi_numeric(x[[time_col]]) else seq_len(nrow(x))
  reference$time <- time
  reference$window_id <- .mi_roll_groups(time, window)
  reference$error_x <- reference$observed_x - reference$reference_x
  reference$error_y <- reference$observed_y - reference$reference_y
  reference$error_distance <- sqrt(reference$error_x^2 + reference$error_y^2)
  summary <- aggregate(cbind(error_x, error_y, error_distance) ~ window_id, data = reference, FUN = .mi_safe_mean)
  summary$n <- as.integer(table(reference$window_id)[as.character(summary$window_id)])
  summary$drift_from_first <- sqrt((summary$error_x - summary$error_x[[1L]])^2 + (summary$error_y - summary$error_y[[1L]])^2)
  threshold <- .mi_safe_quantile(summary$drift_from_first, 0.95, default = 0)
  summary$review_flag <- summary$drift_from_first > max(threshold, 2 * .mi_safe_sd(reference$error_distance))
  .mi_new(
    "eye_calibration_drift",
    observations = reference,
    summary = summary,
    method = method,
    coordinate_columns = c(x = x_col, y = y_col),
    time_col = time_col,
    status = "Calibration drift summarized over time windows."
  )
}

.mi_robust_lm <- function(formula, data, robust = TRUE, iterations = 5L) {
  fit <- stats::lm(formula, data = data)
  if (!isTRUE(robust)) return(fit)
  for (i in seq_len(iterations)) {
    residual <- stats::residuals(fit)
    scale <- stats::mad(residual, constant = 1.4826, na.rm = TRUE)
    if (!is.finite(scale) || scale <= 0) break
    weight <- pmin(1, 1.345 * scale / pmax(abs(residual), 1e-12))
    fit <- stats::lm(formula, data = data, weights = weight)
  }
  fit
}

#' Fit an offline gaze recalibration transform
#'
#' @param x Gaze data with reference coordinates, calibration-drift object, or paired table.
#' @param method Translation, affine, or polynomial transform.
#' @param robust Use iterative Huber-like weights.
#' @param x_col,y_col Observed coordinate columns.
#' @param reference_x_col,reference_y_col Reference columns.
#' @return An `eye_recalibration_model` object.
#' @export
fit_offline_recalibration <- function(
    x,
    method = c("translation", "affine", "polynomial"),
    robust = TRUE,
    x_col = NULL,
    y_col = NULL,
    reference_x_col = NULL,
    reference_y_col = NULL) {
  method <- match.arg(method)
  data <- if (inherits(x, "eye_calibration_drift")) x$observations else x
  .mi_assert_data(data, min_rows = 3L)
  if (inherits(x, "eye_calibration_drift")) {
    columns <- c("observed_x", "observed_y", "reference_x", "reference_y")
  } else {
    x_col <- x_col %||% .mi_first_column(data, c("observed_x", "x", "gaze_x"), label = "observed x")
    y_col <- y_col %||% .mi_first_column(data, c("observed_y", "y", "gaze_y"), label = "observed y")
    reference_x_col <- reference_x_col %||% .mi_first_column(data, c("reference_x", "target_x", "expected_x"), label = "reference x")
    reference_y_col <- reference_y_col %||% .mi_first_column(data, c("reference_y", "target_y", "expected_y"), label = "reference y")
    columns <- c(x_col, y_col, reference_x_col, reference_y_col)
    names(columns) <- c("observed_x", "observed_y", "reference_x", "reference_y")
    data <- data.frame(observed_x = .mi_numeric(data[[x_col]]), observed_y = .mi_numeric(data[[y_col]]),
                       reference_x = .mi_numeric(data[[reference_x_col]]), reference_y = .mi_numeric(data[[reference_y_col]]))
  }
  data <- .mi_complete(data, c("observed_x", "observed_y", "reference_x", "reference_y"))
  if (method == "translation") {
    offset <- c(x = .mi_safe_mean(data$reference_x - data$observed_x), y = .mi_safe_mean(data$reference_y - data$observed_y))
    models <- NULL
  } else {
    formula_x <- if (method == "affine") reference_x ~ observed_x + observed_y else reference_x ~ observed_x + observed_y + I(observed_x^2) + I(observed_y^2) + I(observed_x * observed_y)
    formula_y <- if (method == "affine") reference_y ~ observed_x + observed_y else reference_y ~ observed_x + observed_y + I(observed_x^2) + I(observed_y^2) + I(observed_x * observed_y)
    models <- list(x = .mi_robust_lm(formula_x, data, robust), y = .mi_robust_lm(formula_y, data, robust))
    offset <- NULL
  }
  .mi_new(
    "eye_recalibration_model",
    method = method,
    robust = isTRUE(robust),
    offset = offset,
    models = models,
    training = data,
    status = paste(paste0(toupper(substr(method, 1L, 1L)), substr(method, 2L, nchar(method))), "offline recalibration fitted.")
  )
}

#' Apply an offline recalibration model
#'
#' @param x Data frame with observed coordinates.
#' @param model Recalibration model.
#' @param x_col,y_col Coordinate columns.
#' @param suffix Suffix for corrected coordinates.
#' @return Data frame with corrected coordinates.
#' @export
apply_offline_recalibration <- function(x, model, x_col = NULL, y_col = NULL, suffix = "_recalibrated") {
  .mi_assert_data(x)
  if (!inherits(model, "eye_recalibration_model")) .mi_stop("`model` must be an `eye_recalibration_model`.")
  x_col <- x_col %||% .mi_first_column(x, c("observed_x", "x", "gaze_x"), label = "observed x")
  y_col <- y_col %||% .mi_first_column(x, c("observed_y", "y", "gaze_y"), label = "observed y")
  newdata <- data.frame(observed_x = .mi_numeric(x[[x_col]]), observed_y = .mi_numeric(x[[y_col]]))
  if (model$method == "translation") {
    corrected_x <- newdata$observed_x + model$offset[["x"]]
    corrected_y <- newdata$observed_y + model$offset[["y"]]
  } else {
    corrected_x <- as.numeric(stats::predict(model$models$x, newdata = newdata))
    corrected_y <- as.numeric(stats::predict(model$models$y, newdata = newdata))
  }
  out <- x
  out[[paste0(x_col, suffix)]] <- corrected_x
  out[[paste0(y_col, suffix)]] <- corrected_y
  attr(out, "eye_recalibration_model") <- model
  out
}

#' Audit recalibration improvement
#'
#' @param before Before-calibration paired data or calibration-drift object.
#' @param after Corrected paired data.
#' @param minimum_improvement Optional minimum relative RMSE improvement.
#' @return An `eye_recalibration_audit` object.
#' @export
audit_recalibration <- function(before, after, minimum_improvement = NULL) {
  before_data <- if (inherits(before, "eye_calibration_drift")) before$observations else before
  .mi_assert_data(before_data)
  .mi_assert_data(after)
  before_cols <- c(
    .mi_first_column(before_data, c("observed_x", "x", "gaze_x")),
    .mi_first_column(before_data, c("observed_y", "y", "gaze_y")),
    .mi_first_column(before_data, c("reference_x", "target_x", "expected_x")),
    .mi_first_column(before_data, c("reference_y", "target_y", "expected_y"))
  )
  after_x <- .mi_first_column(after, c("x_recalibrated", "gaze_x_recalibrated", "observed_x_recalibrated", "corrected_x"), label = "corrected x")
  after_y <- .mi_first_column(after, c("y_recalibrated", "gaze_y_recalibrated", "observed_y_recalibrated", "corrected_y"), label = "corrected y")
  reference_x <- .mi_numeric(before_data[[before_cols[[3L]]]])
  reference_y <- .mi_numeric(before_data[[before_cols[[4L]]]])
  before_error <- sqrt((.mi_numeric(before_data[[before_cols[[1L]]]]) - reference_x)^2 + (.mi_numeric(before_data[[before_cols[[2L]]]]) - reference_y)^2)
  after_error <- sqrt((.mi_numeric(after[[after_x]]) - reference_x)^2 + (.mi_numeric(after[[after_y]]) - reference_y)^2)
  before_rmse <- sqrt(mean(before_error^2, na.rm = TRUE))
  after_rmse <- sqrt(mean(after_error^2, na.rm = TRUE))
  improvement <- (before_rmse - after_rmse) / pmax(before_rmse, 1e-12)
  passed <- if (is.null(minimum_improvement)) after_rmse < before_rmse else improvement >= minimum_improvement
  .mi_new(
    "eye_recalibration_audit",
    errors = data.frame(before = before_error, after = after_error),
    summary = data.frame(before_rmse = before_rmse, after_rmse = after_rmse, relative_improvement = improvement, passed = passed),
    minimum_improvement = minimum_improvement,
    status = if (passed) "Recalibration improved spatial accuracy." else "Recalibration did not meet the improvement criterion."
  )
}

#' @export
plot.eye_calibration_drift <- function(x, type = c("drift_over_time", "vector_field", "error_ellipses", "screen_coverage"), ...) {
  type <- match.arg(type)
  if (type == "drift_over_time") {
    graphics::plot(x$summary$window_id, x$summary$drift_from_first, type = "b", xlab = "Window", ylab = "Drift distance", main = "Calibration drift over time")
  } else if (type == "vector_field") {
    d <- x$observations
    graphics::plot(d$reference_x, d$reference_y, asp = 1, xlab = "Reference x", ylab = "Reference y", main = "Calibration error vector field")
    graphics::arrows(d$reference_x, d$reference_y, d$observed_x, d$observed_y, length = 0.05)
  } else if (type == "error_ellipses") {
    d <- x$observations
    graphics::plot(d$error_x, d$error_y, asp = 1, xlab = "Horizontal error", ylab = "Vertical error", main = "Calibration error cloud")
    graphics::abline(h = 0, v = 0, lty = 2)
  } else {
    d <- x$observations
    graphics::plot(d$observed_x, d$observed_y, asp = 1, xlab = "Observed x", ylab = "Observed y", main = "Screen coverage")
  }
  invisible(x)
}

#' @export
plot.eye_recalibration_audit <- function(x, type = c("before_after", "diagnostics"), ...) {
  type <- match.arg(type)
  if (type == "before_after") {
    graphics::boxplot(x$errors, ylab = "Spatial error", main = "Recalibration before and after")
  } else {
    graphics::plot(x$errors$before, x$errors$after, xlab = "Before error", ylab = "After error", main = "Recalibration audit")
    graphics::abline(0, 1, lty = 2)
  }
  invisible(x)
}

#' @export
plot_calibration_vector_field <- function(x, ...) plot(x, type = "vector_field", ...)
#' @export
plot_calibration_error_ellipses <- function(x, ...) plot(x, type = "error_ellipses", ...)
#' @export
plot_drift_over_time <- function(x, ...) plot(x, type = "drift_over_time", ...)
#' @export
plot_recalibration_before_after <- function(x, ...) plot(x, type = "before_after", ...)
#' @export
plot_screen_coverage <- function(x, ...) plot(x, type = "screen_coverage", ...)
