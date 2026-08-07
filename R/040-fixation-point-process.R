# Spatio-temporal fixation point-process models -------------------------------

#' Fit a dependency-free fixation point-process approximation
#'
#' @param x Fixation table.
#' @param spatial_covariates Optional spatial covariate columns.
#' @param temporal_covariates Optional temporal covariate columns.
#' @param interaction None or self-exciting history term.
#' @param x_col,y_col,time_col Coordinate and time columns.
#' @param grid_size Spatial grid resolution.
#' @return An `eye_fixation_point_process` object.
#' @export
fit_fixation_point_process <- function(
    x,
    spatial_covariates = NULL,
    temporal_covariates = NULL,
    interaction = c("none", "self_exciting"),
    x_col = "x",
    y_col = "y",
    time_col = "time",
    grid_size = 20) {
  .mi_assert_data(x, min_rows = 5L)
  interaction <- match.arg(interaction)
  .mi_assert_columns(x, c(x_col, y_col))
  data <- x
  data$.x <- .mi_numeric(data[[x_col]])
  data$.y <- .mi_numeric(data[[y_col]])
  data$.time <- if (time_col %in% names(data)) .mi_numeric(data[[time_col]]) else seq_len(nrow(data))
  data <- data[stats::complete.cases(data[, c(".x", ".y", ".time")]), , drop = FALSE]
  if (nrow(data) < 5L) .mi_stop("At least five complete fixation observations are required.")
  grid_size <- as.integer(grid_size)
  if (!is.finite(grid_size) || grid_size < 2L) .mi_stop("`grid_size` must be an integer of at least 2.")
  expand_range <- function(values) {
    value_range <- range(values, finite = TRUE)
    if (!all(is.finite(value_range))) .mi_stop("Fixation coordinates must contain finite values.")
    if (diff(value_range) == 0) {
      padding <- max(abs(value_range[[1L]]) * 0.01, 0.5)
      value_range <- value_range + c(-padding, padding)
    }
    value_range
  }
  x_range <- expand_range(data$.x)
  y_range <- expand_range(data$.y)
  x_breaks <- seq(x_range[[1L]], x_range[[2L]], length.out = grid_size + 1L)
  y_breaks <- seq(y_range[[1L]], y_range[[2L]], length.out = grid_size + 1L)
  data$.x_bin <- cut(data$.x, breaks = x_breaks, include.lowest = TRUE, labels = FALSE)
  data$.y_bin <- cut(data$.y, breaks = y_breaks, include.lowest = TRUE, labels = FALSE)
  counts <- aggregate(rep(1, nrow(data)), by = list(x_bin = data$.x_bin, y_bin = data$.y_bin), FUN = sum)
  names(counts)[[3L]] <- "count"
  counts$x_center <- (x_breaks[counts$x_bin] + x_breaks[pmin(counts$x_bin + 1L, length(x_breaks))]) / 2
  counts$y_center <- (y_breaks[counts$y_bin] + y_breaks[pmin(counts$y_bin + 1L, length(y_breaks))]) / 2
  if (interaction == "self_exciting") {
    ordered <- data[order(data$.time), ]
    lag_distance <- c(NA_real_, sqrt(diff(ordered$.x)^2 + diff(ordered$.y)^2))
    scale_distance <- .mi_safe_mean(lag_distance)
    if (!is.finite(scale_distance) || scale_distance <= 0) scale_distance <- 1
    history <- data.frame(x_bin = ordered$.x_bin, y_bin = ordered$.y_bin, history = exp(-lag_distance / scale_distance))
    history <- aggregate(history ~ x_bin + y_bin, data = history, FUN = .mi_safe_mean)
    counts <- merge(counts, history, by = c("x_bin", "y_bin"), all.x = TRUE)
    counts$history[!is.finite(counts$history)] <- 0
  }
  requested_covariates <- unique(c(spatial_covariates, temporal_covariates))
  requested_covariates <- requested_covariates[requested_covariates %in% names(data)]
  covariate_map <- data.frame(source = character(), internal = character(), stringsAsFactors = FALSE)
  if (length(requested_covariates)) {
    for (index in seq_along(requested_covariates)) {
      source_name <- requested_covariates[[index]]
      internal_name <- paste0("covariate_", index)
      values <- .mi_numeric(data[[source_name]])
      covariate_table <- aggregate(
        values,
        by = list(x_bin = data$.x_bin, y_bin = data$.y_bin),
        FUN = .mi_safe_mean
      )
      names(covariate_table)[[3L]] <- internal_name
      counts <- merge(counts, covariate_table, by = c("x_bin", "y_bin"), all.x = TRUE)
      replacement <- .mi_safe_mean(counts[[internal_name]])
      if (!is.finite(replacement)) replacement <- 0
      counts[[internal_name]][!is.finite(counts[[internal_name]])] <- replacement
      covariate_map <- rbind(covariate_map, data.frame(source = source_name, internal = internal_name, stringsAsFactors = FALSE))
    }
  }
  predictors <- c("x_center", "y_center", "I(x_center^2)", "I(y_center^2)", "I(x_center * y_center)")
  if (interaction == "self_exciting") predictors <- c(predictors, "history")
  predictors <- c(predictors, covariate_map$internal)
  formula <- stats::as.formula(paste("count ~", paste(predictors, collapse = " + ")))
  model <- stats::glm(formula, data = counts, family = stats::poisson())
  counts$expected <- as.numeric(stats::predict(model, type = "response"))
  counts$residual <- counts$count - counts$expected
  .mi_new(
    "eye_fixation_point_process",
    model = model,
    data = data,
    grid = counts,
    x_breaks = x_breaks,
    y_breaks = y_breaks,
    interaction = interaction,
    spatial_covariates = spatial_covariates,
    temporal_covariates = temporal_covariates,
    covariate_map = covariate_map,
    predictor_defaults = setNames(vapply(covariate_map$internal, function(name) .mi_safe_mean(counts[[name]]), numeric(1)), covariate_map$internal),
    summary = as.data.frame(summary(model)$coefficients),
    status = "Fixation intensity fitted as a gridded Poisson point-process approximation."
  )
}

#' Fit marked gaze-process models
#'
#' @param x Fixation table.
#' @param marks Mark columns such as duration, pupil, and saccade amplitude.
#' @param x_col,y_col,time_col Coordinate and time columns.
#' @return An `eye_marked_gaze_process` object.
#' @export
fit_marked_gaze_process <- function(x, marks = c("duration", "pupil", "saccade_amplitude"), x_col = "x", y_col = "y", time_col = "time") {
  .mi_assert_data(x, min_rows = 5L)
  .mi_assert_columns(x, c(x_col, y_col))
  marks <- marks[marks %in% names(x)]
  if (!length(marks)) .mi_stop("No requested mark columns are available.")
  data <- x
  data$.x <- .mi_numeric(data[[x_col]]); data$.y <- .mi_numeric(data[[y_col]])
  data$.time <- if (time_col %in% names(data)) .mi_numeric(data[[time_col]]) else seq_len(nrow(data))
  models <- lapply(marks, function(mark) {
    data$.mark <- .mi_numeric(data[[mark]])
    stats::lm(.mark ~ .x + .y + .time + I(.x * .y), data = data)
  })
  names(models) <- marks
  summary <- do.call(rbind, lapply(marks, function(mark) {
    coefficients <- as.data.frame(summary(models[[mark]])$coefficients)
    coefficients$term <- rownames(coefficients); coefficients$mark <- mark; rownames(coefficients) <- NULL
    coefficients
  }))
  .mi_new("eye_marked_gaze_process", models = models, data = data, marks = marks, summary = summary,
          status = "Marked gaze-process models fitted.")
}

#' Predict fixation intensity
#'
#' @param model Fixation point-process object.
#' @param new_stimulus Optional prediction grid.
#' @return Data frame with predicted intensity.
#' @export
predict_fixation_intensity <- function(model, new_stimulus = NULL) {
  if (!inherits(model, "eye_fixation_point_process")) .mi_stop("`model` must be an `eye_fixation_point_process` object.")
  data <- new_stimulus %||% model$grid
  .mi_assert_data(data)
  if (!all(c("x_center", "y_center") %in% names(data))) {
    x_col <- .mi_first_column(data, c("x_center", "x", "gaze_x")); y_col <- .mi_first_column(data, c("y_center", "y", "gaze_y"))
    data$x_center <- .mi_numeric(data[[x_col]]); data$y_center <- .mi_numeric(data[[y_col]])
  }
  if (model$interaction == "self_exciting" && !"history" %in% names(data)) data$history <- 0
  if (nrow(model$covariate_map)) {
    for (i in seq_len(nrow(model$covariate_map))) {
      source_name <- model$covariate_map$source[[i]]
      internal_name <- model$covariate_map$internal[[i]]
      if (source_name %in% names(data)) {
        data[[internal_name]] <- .mi_numeric(data[[source_name]])
      } else if (!internal_name %in% names(data)) {
        data[[internal_name]] <- model$predictor_defaults[[internal_name]] %||% 0
      }
    }
  }
  data$predicted_intensity <- as.numeric(stats::predict(model$model, newdata = data, type = "response"))
  data
}

#' Diagnose a gaze point-process model
#'
#' @param model Fixation point-process object.
#' @return An `eye_gaze_point_process_diagnostics` object.
#' @export
diagnose_gaze_point_process <- function(model) {
  if (!inherits(model, "eye_fixation_point_process")) .mi_stop("`model` must be an `eye_fixation_point_process` object.")
  grid <- model$grid
  pearson <- (grid$count - grid$expected) / sqrt(pmax(grid$expected, 1e-8))
  summary <- data.frame(
    mean_pearson = .mi_safe_mean(pearson),
    sd_pearson = .mi_safe_sd(pearson),
    overdispersion = sum(pearson^2, na.rm = TRUE) / max(1, nrow(grid) - length(stats::coef(model$model))),
    correlation_observed_expected = stats::cor(grid$count, grid$expected),
    stringsAsFactors = FALSE
  )
  .mi_new("eye_gaze_point_process_diagnostics", model = model, residuals = pearson, summary = summary,
          status = "Point-process diagnostics calculated.")
}

#' @export
plot.eye_fixation_point_process <- function(x, type = c("intensity", "spatial_residuals", "excitation", "covariate_surface", "observed_expected", "diagnostics"), ...) {
  type <- match.arg(type)
  grid <- x$grid
  if (type %in% c("intensity", "covariate_surface")) {
    z <- xtabs(expected ~ x_bin + y_bin, data = grid)
    graphics::image(as.numeric(rownames(z)), as.numeric(colnames(z)), z, xlab = "X grid", ylab = "Y grid", main = "Predicted fixation intensity")
  } else if (type == "spatial_residuals" || type == "diagnostics") {
    graphics::plot(grid$x_center, grid$y_center, cex = 0.5 + 2 * .mi_rescale01(abs(grid$residual)), xlab = "X", ylab = "Y", main = "Spatial point-process residuals")
  } else if (type == "observed_expected") {
    graphics::plot(grid$expected, grid$count, xlab = "Expected fixations", ylab = "Observed fixations", main = "Observed versus expected fixations")
    graphics::abline(0, 1, lty = 2)
  } else {
    if (!"history" %in% names(grid)) return(.mi_plot_empty("The model has no self-excitation term."))
    graphics::plot(grid$history, grid$expected, xlab = "History term", ylab = "Expected intensity", main = "Temporal excitation kernel proxy")
  }
  invisible(x)
}
#' @export
plot.eye_gaze_point_process_diagnostics <- function(x, type = c("diagnostics", "observed_expected"), ...) {
  if (type[[1L]] == "observed_expected") plot(x$model, type = "observed_expected", ...) else graphics::hist(x$residuals, xlab = "Pearson residual", main = "Point-process residual diagnostics")
  invisible(x)
}
#' @export
plot_fixation_intensity <- function(x, ...) plot(x, type = "intensity", ...)
#' @export
plot_spatial_residuals <- function(x, ...) plot(x, type = "spatial_residuals", ...)
#' @export
plot_temporal_excitation_kernel <- function(x, ...) plot(x, type = "excitation", ...)
#' @export
plot_covariate_effect_surface <- function(x, ...) plot(x, type = "covariate_surface", ...)
#' @export
plot_observed_expected_fixations <- function(x, ...) plot(x, type = "observed_expected", ...)
