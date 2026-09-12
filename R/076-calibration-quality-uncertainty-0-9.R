# eyeprocess 0.9.0.9000 ------------------------------------------------------
# Calibration uncertainty, empirical data-quality metrics, and probabilistic AOIs.

.ep09_group_split <- function(data, by = NULL) {
  if (is.null(by) || !length(by)) return(list(all = seq_len(nrow(data))))
  .ep09_req_cols(data, by, "data")
  split(seq_len(nrow(data)), interaction(data[by], drop = TRUE, lex.order = TRUE))
}

.ep09_group_header <- function(z, by) {
  if (is.null(by) || !length(by)) return(data.frame(.group = "all", stringsAsFactors = FALSE))
  z[1L, by, drop = FALSE]
}

#' Estimate empirical calibration/validation error
#'
#' @param data Validation-target data.
#' @param gaze_x,gaze_y Recorded gaze-coordinate columns.
#' @param target_x,target_y Known target-coordinate columns.
#' @param by Optional grouping columns such as participant/session.
#' @return A tabular R object containing empirical calibration/validation error; rows represent analysis units and columns contain the returned quantities.
#' @export
estimate_calibration_error <- function(data, gaze_x = "gaze_x", gaze_y = "gaze_y",
                                       target_x = "target_x", target_y = "target_y", by = NULL) {
  d <- .ep09_as_df(data); .ep09_req_cols(d, c(gaze_x, gaze_y, target_x, target_y, by), "data")
  rows <- lapply(.ep09_group_split(d, by), function(idx) {
    z <- d[idx, , drop = FALSE]
    dx <- .ep09_num(z[[gaze_x]]) - .ep09_num(z[[target_x]])
    dy <- .ep09_num(z[[gaze_y]]) - .ep09_num(z[[target_y]])
    rad <- sqrt(dx^2 + dy^2); ok <- is.finite(rad)
    dx_f <- dx[is.finite(dx)]; dy_f <- dy[is.finite(dy)]; rad_f <- rad[ok]
    cbind(.ep09_group_header(z, by), data.frame(
      n = sum(ok), bias_x = if (length(dx_f)) mean(dx_f) else NA_real_, bias_y = if (length(dy_f)) mean(dy_f) else NA_real_,
      mean_radial_error = if (length(rad_f)) mean(rad_f) else NA_real_, median_radial_error = if (length(rad_f)) stats::median(rad_f) else NA_real_,
      rms_radial_error = if (length(rad_f)) sqrt(mean(rad_f^2)) else NA_real_, p95_radial_error = .ep09_quantile(rad_f, .95),
      stringsAsFactors = FALSE
    ))
  })
  .ep09_rbind_fill(rows)
}

#' Estimate RMS successive-sample gaze imprecision
#'
#' @param data Gaze samples.
#' @param x,y Gaze-coordinate columns.
#' @param time Optional timestamp column used to order samples.
#' @param by Optional grouping columns.
#' @return A logical value or vector indicating rMS successive-sample gaze imprecision.
#' @export
gaze_precision_rms_s2s <- function(data, x = "gaze_x", y = "gaze_y", time = NULL, by = NULL) {
  d <- .ep09_as_df(data); .ep09_req_cols(d, c(x, y, time, by), "data")
  .ep09_rbind_fill(lapply(.ep09_group_split(d, by), function(idx) {
    z <- d[idx, , drop = FALSE]
    if (!is.null(time)) z <- z[order(.ep09_num(z[[time]])), , drop = FALSE]
    xx <- .ep09_num(z[[x]]); yy <- .ep09_num(z[[y]])
    step <- sqrt(diff(xx)^2 + diff(yy)^2); step <- step[is.finite(step)]
    cbind(.ep09_group_header(z, by), data.frame(
      n_steps = length(step), rms_s2s = if (length(step)) sqrt(mean(step^2)) else NA_real_,
      median_s2s = if (length(step)) stats::median(step) else NA_real_,
      p95_s2s = .ep09_quantile(step, .95), stringsAsFactors = FALSE
    ))
  }))
}

#' Estimate effective sampling frequency from timestamps
#' @param data Sample data.
#' @param time Timestamp column.
#' @param unit Timestamp unit.
#' @param by Optional grouping columns.
#' @return A logical value or vector indicating effective sampling frequency from timestamps.
#' @export
effective_sampling_frequency <- function(data, time = "timestamp_ms",
                                         unit = c("ms", "s", "us"), by = NULL) {
  unit <- match.arg(unit); scale <- switch(unit, ms = 1000, s = 1, us = 1e6)
  d <- .ep09_as_df(data); .ep09_req_cols(d, c(time, by), "data")
  .ep09_rbind_fill(lapply(.ep09_group_split(d, by), function(idx) {
    z <- d[idx, , drop = FALSE]; tt <- sort(.ep09_num(z[[time]])); dt <- diff(tt); dt <- dt[is.finite(dt) & dt > 0]
    med <- if (length(dt)) stats::median(dt) else NA_real_
    cbind(.ep09_group_header(z, by), data.frame(
      n_intervals = length(dt), median_interval = med,
      effective_hz = if (is.finite(med) && med > 0) scale / med else NA_real_,
      interval_cv = if (length(dt) > 1L && mean(dt) != 0) stats::sd(dt) / mean(dt) else NA_real_,
      stringsAsFactors = FALSE
    ))
  }))
}

#' Audit sampling irregularity
#' @param data Sample data.
#' @param time Timestamp column.
#' @param unit Timestamp unit.
#' @param by Optional grouping columns.
#' @param cv_threshold Review threshold for interval coefficient of variation.
#' @return An object of class "eye_sampling_irregularity_audit", stored as a named list, with components "table", "cv_threshold", "caveat". It contains sampling irregularity and associated metadata or diagnostics needed to interpret the result.
#' @export
audit_sampling_irregularity <- function(data, time = "timestamp_ms", unit = c("ms", "s", "us"),
                                        by = NULL, cv_threshold = .05) {
  if (length(cv_threshold) != 1L || !is.finite(cv_threshold) || cv_threshold < 0) stop("cv_threshold must be a finite non-negative scalar.", call. = FALSE)
  tab <- effective_sampling_frequency(data, time, unit, by)
  tab$irregularity_flag <- is.finite(tab$interval_cv) & tab$interval_cv > cv_threshold
  structure(list(table = tab, cv_threshold = cv_threshold,
                 caveat = "Sampling irregularity thresholds are workflow-specific review rules, not universal acceptability cutoffs."),
            class = "eye_sampling_irregularity_audit")
}

#' Build an empirical bivariate calibration-error model
#' @param data Validation-target data.
#' @inheritParams estimate_calibration_error
#' @return An object of class "eye_calibration_error_model", stored as a named list, with components "mean_error", "covariance", "errors", "n", "metrics", "coordinate_units", "status", "caveat". It contains an empirical bivariate calibration-error model and associated metadata or diagnostics needed to interpret the result.
#' @export
calibration_error_model <- function(data, gaze_x = "gaze_x", gaze_y = "gaze_y",
                                    target_x = "target_x", target_y = "target_y") {
  d <- .ep09_as_df(data); .ep09_req_cols(d, c(gaze_x, gaze_y, target_x, target_y), "data")
  dx <- .ep09_num(d[[gaze_x]]) - .ep09_num(d[[target_x]])
  dy <- .ep09_num(d[[gaze_y]]) - .ep09_num(d[[target_y]])
  ok <- is.finite(dx) & is.finite(dy); E <- cbind(dx[ok], dy[ok])
  if (nrow(E) < 3L) stop("At least three complete calibration-error pairs are required.", call. = FALSE)
  structure(list(
    mean_error = colMeans(E), covariance = stats::cov(E), errors = E,
    n = nrow(E), metrics = estimate_calibration_error(d, gaze_x, gaze_y, target_x, target_y),
    coordinate_units = "input_coordinate_units",
    status = "empirical_calibration_error_model",
    caveat = "The model approximates observed calibration/validation error and should be estimated in the coordinate system used for downstream AOIs."
  ), class = "eye_calibration_error_model")
}

#' Uncertainty ellipse implied by an empirical calibration-error model
#' @param model Calibration error model.
#' @param level Probability level.
#' @param center Optional center; defaults to model mean error.
#' @return A data frame containing uncertainty ellipse implied by an empirical calibration-error model. Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function.
#' @export
gaze_uncertainty_ellipse <- function(model, level = .95, center = NULL) {
  if (!inherits(model, "eye_calibration_error_model")) stop("model must be an eye_calibration_error_model.", call. = FALSE)
  if (length(level) != 1L || !is.finite(level) || level <= 0 || level >= 1) stop("level must lie in (0,1).", call. = FALSE)
  ev <- eigen(model$covariance, symmetric = TRUE)
  axes <- sqrt(pmax(ev$values, 0) * stats::qchisq(level, df = 2))
  angle <- atan2(ev$vectors[2L, 1L], ev$vectors[1L, 1L]) * 180 / pi
  if (is.null(center)) center <- model$mean_error
  center <- .ep09_num(center)
  if (length(center) != 2L || any(!is.finite(center))) stop("center must contain two finite coordinates.", call. = FALSE)
  data.frame(center_x = center[[1L]], center_y = center[[2L]], major_axis = max(axes),
             minor_axis = min(axes), angle_deg = angle, level = level, stringsAsFactors = FALSE)
}

#' Propagate empirical calibration uncertainty around gaze samples
#'
#' @param data Gaze samples.
#' @param model Calibration error model.
#' @param x,y Gaze-coordinate columns.
#' @param draws Monte Carlo draws per sample.
#' @param seed Seed.
#' @return A tabular R object containing propagate empirical calibration uncertainty around gaze samples; rows represent analysis units and columns contain the returned quantities.
#' @export
propagate_calibration_uncertainty <- function(data, model, x = "gaze_x", y = "gaze_y",
                                              draws = 500L, seed = 1L) {
  if (!inherits(model, "eye_calibration_error_model")) stop("model must be an eye_calibration_error_model.", call. = FALSE)
  d <- .ep09_as_df(data); .ep09_req_cols(d, c(x, y), "data")
  if (length(draws) != 1L || !is.finite(draws) || draws < 1 || draws != as.integer(draws)) stop("draws must be a positive integer.", call. = FALSE)
  if (length(seed) != 1L || !is.finite(seed) || seed < 0) stop("seed must be a finite non-negative scalar.", call. = FALSE)
  draws <- as.integer(draws)
  S <- model$covariance
  if (any(!is.finite(S))) stop("Calibration covariance contains non-finite values.", call. = FALSE)
  ev <- eigen(S, symmetric = TRUE); root <- ev$vectors %*% diag(sqrt(pmax(ev$values, 0)), 2L) %*% t(ev$vectors)
  set.seed(as.integer((as.double(seed) %% (.Machine$integer.max - 1)) + 1))
  rows <- lapply(seq_len(nrow(d)), function(i) {
    z <- matrix(stats::rnorm(draws * 2L), ncol = 2L) %*% root
    z <- sweep(z, 2L, model$mean_error, "+")
    data.frame(sample_id = i, draw_id = seq_len(draws),
               gaze_x = .ep09_num(d[[x]][i]) - z[, 1L],
               gaze_y = .ep09_num(d[[y]][i]) - z[, 2L], stringsAsFactors = FALSE)
  })
  out <- .ep09_rbind_fill(rows)
  attr(out, "model_n") <- model$n; attr(out, "draws") <- draws
  class(out) <- c("eye_gaze_uncertainty_draws", "data.frame")
  out
}

.ep09_validate_rect_aois <- function(aois) {
  a <- .ep09_as_df(aois); .ep09_req_cols(a, c("aoi", "x_min", "x_max", "y_min", "y_max"), "aois")
  if (!nrow(a)) stop("aois must contain at least one rectangle.", call. = FALSE)
  if (anyNA(a$aoi) || any(!nzchar(as.character(a$aoi))) || anyDuplicated(as.character(a$aoi))) stop("AOI names must be non-missing, non-empty, and unique.", call. = FALSE)
  for (nm in c("x_min", "x_max", "y_min", "y_max")) a[[nm]] <- .ep09_num(a[[nm]])
  if (any(!is.finite(as.matrix(a[c("x_min", "x_max", "y_min", "y_max")]))) ||
      any(a$x_min > a$x_max) || any(a$y_min > a$y_max)) stop("AOI rectangle bounds must be finite and ordered.", call. = FALSE)
  a$aoi <- as.character(a$aoi)
  a
}

#' AOI membership probabilities from uncertainty draws
#' @param draws Output of `propagate_calibration_uncertainty()` or compatible table.
#' @param aois Rectangular AOI table with aoi/x_min/x_max/y_min/y_max.
#' @return A tabular R object containing aOI membership probabilities from uncertainty draws; rows represent analysis units and columns contain the returned quantities.
#' @export
aoi_membership_probability <- function(draws, aois) {
  d <- .ep09_as_df(draws); .ep09_req_cols(d, c("sample_id", "gaze_x", "gaze_y"), "draws")
  a <- .ep09_validate_rect_aois(aois)
  rows <- lapply(seq_len(nrow(a)), function(j) {
    inside <- d$gaze_x >= a$x_min[j] & d$gaze_x <= a$x_max[j] & d$gaze_y >= a$y_min[j] & d$gaze_y <= a$y_max[j]
    p <- tapply(inside, d$sample_id, function(z) if (any(!is.na(z))) mean(z, na.rm = TRUE) else NA_real_)
    data.frame(sample_id = as.integer(names(p)), aoi = as.character(a$aoi[j]), probability = as.numeric(p), stringsAsFactors = FALSE)
  })
  .ep09_rbind_fill(rows)
}

#' Probabilistic AOI assignment under empirical calibration uncertainty
#' @param data Gaze samples.
#' @param aois Rectangular AOI table.
#' @param model Calibration error model.
#' @param x,y Gaze-coordinate columns.
#' @param draws Monte Carlo draws.
#' @param seed Seed.
#' @param min_probability Minimum probability for assignment; lower maxima become `NA`.
#' @return An object of class "eye_probabilistic_aoi_assignment", stored as a named list, with components "assignments", "probabilities", "aois", "model", "min_probability", "caveat". It contains probabilistic AOI assignment under empirical calibration uncertainty and associated metadata or diagnostics needed to interpret the result.
#' @export
probabilistic_aoi_assignment <- function(data, aois, model, x = "gaze_x", y = "gaze_y",
                                         draws = 500L, seed = 1L, min_probability = .5) {
  min_probability <- as.numeric(min_probability)[1L]
  if (!is.finite(min_probability) || min_probability < 0 || min_probability > 1) stop("min_probability must lie in [0,1].", call. = FALSE)
  u <- propagate_calibration_uncertainty(data, model, x, y, draws, seed)
  p <- aoi_membership_probability(u, aois)
  spl <- split(p, p$sample_id)
  assn <- .ep09_rbind_fill(lapply(spl, function(z) {
    sample_id <- z$sample_id[[1L]]
    z <- z[is.finite(z$probability), , drop = FALSE]
    if (!nrow(z)) return(data.frame(sample_id = sample_id, aoi = NA_character_, probability = NA_real_, ambiguity = NA_real_, stringsAsFactors = FALSE))
    z <- z[order(-z$probability), , drop = FALSE]; best <- z[1L, , drop = FALSE]
    data.frame(sample_id = best$sample_id, aoi = if (best$probability >= min_probability) best$aoi else NA_character_,
               probability = best$probability,
               ambiguity = if (nrow(z) > 1L) best$probability - z$probability[2L] else best$probability,
               stringsAsFactors = FALSE)
  }))
  structure(list(assignments = assn, probabilities = p, aois = aois, model = model,
                 min_probability = min_probability,
                 caveat = "Probabilities quantify propagated calibration uncertainty under the fitted error model; they are not posterior probabilities of psychological attention."),
            class = "eye_probabilistic_aoi_assignment")
}

#' Compare hard and probabilistic AOI assignments
#' @param data Gaze samples.
#' @param aois Rectangular AOIs.
#' @param probabilistic Probabilistic assignment object.
#' @param x,y Gaze-coordinate columns.
#' @return An R object containing hard and probabilistic AOI assignments. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
compare_hard_probabilistic_aoi <- function(data, aois, probabilistic, x = "gaze_x", y = "gaze_y") {
  d <- .ep09_as_df(data); a <- .ep09_validate_rect_aois(aois); .ep09_req_cols(d, c(x, y), "data")
  if (!inherits(probabilistic, "eye_probabilistic_aoi_assignment")) stop("probabilistic must be an eye_probabilistic_aoi_assignment.", call. = FALSE)
  hard <- vapply(seq_len(nrow(d)), function(i) {
    hit <- which(.ep09_num(d[[x]][i]) >= a$x_min & .ep09_num(d[[x]][i]) <= a$x_max &
                 .ep09_num(d[[y]][i]) >= a$y_min & .ep09_num(d[[y]][i]) <= a$y_max)
    if (length(hit)) as.character(a$aoi[hit[[1L]]]) else NA_character_
  }, character(1))
  pr <- probabilistic$assignments
  out <- data.frame(sample_id = seq_len(nrow(d)), hard_aoi = hard, stringsAsFactors = FALSE)
  out <- merge(out, pr, by = "sample_id", all.x = TRUE, sort = FALSE)
  names(out)[names(out) == "aoi"] <- "probabilistic_aoi"
  out$agreement <- (is.na(out$hard_aoi) & is.na(out$probabilistic_aoi)) |
    (!is.na(out$hard_aoi) & !is.na(out$probabilistic_aoi) & out$hard_aoi == out$probabilistic_aoi)
  out
}

#' Sensitivity grid for deterministic calibration offsets
#' @param offset_x,offset_y Candidate offsets in coordinate units.
#' @return An R object containing sensitivity grid for deterministic calibration offsets. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
calibration_sensitivity_grid <- function(offset_x = c(-.02, 0, .02), offset_y = c(-.02, 0, .02)) {
  offset_x <- .ep09_num(offset_x); offset_y <- .ep09_num(offset_y)
  if (!length(offset_x) || !length(offset_y) || any(!is.finite(offset_x)) || any(!is.finite(offset_y))) stop("offset_x and offset_y must contain finite values.", call. = FALSE)
  g <- expand.grid(offset_x = offset_x, offset_y = offset_y, KEEP.OUT.ATTRS = FALSE)
  g$radial_offset <- sqrt(g$offset_x^2 + g$offset_y^2)
  g$calibration_spec_id <- sprintf("CAL%03d", seq_len(nrow(g)))
  g[, c("calibration_spec_id", setdiff(names(g), "calibration_spec_id")), drop = FALSE]
}

#' Distance to nearest rectangular AOI boundary
#' @param data Gaze samples.
#' @param aois Rectangular AOIs.
#' @param x,y Coordinates.
#' @return A tabular R object containing distance to nearest rectangular AOI boundary; rows represent analysis units and columns contain the returned quantities.
#' @export
fixation_boundary_uncertainty <- function(data, aois, x = "gaze_x", y = "gaze_y") {
  d <- .ep09_as_df(data); a <- .ep09_validate_rect_aois(aois); .ep09_req_cols(d, c(x, y), "data")
  rows <- lapply(seq_len(nrow(d)), function(i) {
    xx <- .ep09_num(d[[x]][i]); yy <- .ep09_num(d[[y]][i])
    if (!is.finite(xx) || !is.finite(yy)) {
      return(data.frame(sample_id = i, nearest_aoi = NA_character_, signed_boundary_distance = NA_real_, stringsAsFactors = FALSE))
    }
    dist <- vapply(seq_len(nrow(a)), function(j) {
      dx <- max(a$x_min[j] - xx, 0, xx - a$x_max[j]); dy <- max(a$y_min[j] - yy, 0, yy - a$y_max[j])
      if (dx == 0 && dy == 0) min(xx - a$x_min[j], a$x_max[j] - xx, yy - a$y_min[j], a$y_max[j] - yy) else -sqrt(dx^2 + dy^2)
    }, numeric(1))
    j <- which.max(dist)
    data.frame(sample_id = i, nearest_aoi = a$aoi[j], signed_boundary_distance = dist[j], stringsAsFactors = FALSE)
  })
  .ep09_rbind_fill(rows)
}

#' Calibration drift profile across sessions/batches
#' @param data Validation-target data.
#' @param by Ordered batch/session column.
#' @inheritParams estimate_calibration_error
#' @return An object of class "eye_calibration_drift_profile", stored as a named list, with components "table", "by", "caveat". It contains calibration drift profile across sessions/batches and associated metadata or diagnostics needed to interpret the result.
#' @export
calibration_drift_profile <- function(data, by, gaze_x = "gaze_x", gaze_y = "gaze_y",
                                      target_x = "target_x", target_y = "target_y") {
  tab <- estimate_calibration_error(data, gaze_x, gaze_y, target_x, target_y, by = by)
  if (!nrow(tab)) stop("No calibration groups are available for drift profiling.", call. = FALSE)
  tab$baseline_error <- tab$mean_radial_error[[1L]]
  tab$delta_from_first <- tab$mean_radial_error - tab$baseline_error
  structure(list(table = tab, by = by,
                 caveat = "Observed drift may reflect calibration, participant movement, geometry, lighting, hardware, or other acquisition changes."),
            class = "eye_calibration_drift_profile")
}

#' Empirical gaze data-quality profile
#'
#' @param data Gaze samples.
#' @param x,y Gaze coordinates.
#' @param time Timestamp column.
#' @param target_x,target_y Optional known target coordinates.
#' @param valid Optional validity indicator column.
#' @param by Optional grouping columns.
#' @param time_unit Timestamp unit.
#' @return An object of class "eye_data_quality_profile", stored as a named list, with components "table", "coordinate_units", "caveat". It contains empirical gaze data-quality profile and associated metadata or diagnostics needed to interpret the result.
#' @export
gaze_data_quality_profile <- function(data, x = "gaze_x", y = "gaze_y", time = "timestamp_ms",
                                      target_x = NULL, target_y = NULL, valid = NULL, by = NULL,
                                      time_unit = c("ms", "s", "us")) {
  time_unit <- match.arg(time_unit); d <- .ep09_as_df(data); .ep09_req_cols(d, c(x, y, time, valid, by), "data")
  if (xor(is.null(target_x), is.null(target_y))) stop("target_x and target_y must be supplied together.", call. = FALSE)
  if (!nrow(d)) return(structure(list(table = data.frame(), coordinate_units = "input_coordinate_units",
                                     caveat = "No samples were supplied; data-quality metrics are undefined."), class = "eye_data_quality_profile"))
  groups <- .ep09_group_split(d, by)
  rows <- lapply(groups, function(idx) {
    z <- d[idx, , drop = FALSE]
    complete_xy <- is.finite(.ep09_num(z[[x]])) & is.finite(.ep09_num(z[[y]]))
    valid_vec <- if (!is.null(valid)) {
      vv <- as.logical(z[[valid]])
      !is.na(vv) & vv & complete_xy
    } else complete_xy
    pr <- gaze_precision_rms_s2s(z[valid_vec, , drop = FALSE], x, y, time, by = NULL)
    ef <- effective_sampling_frequency(z, time, time_unit, by = NULL)
    acc <- if (!is.null(target_x) && !is.null(target_y) && all(c(target_x, target_y) %in% names(z)))
      estimate_calibration_error(z, x, y, target_x, target_y, by = NULL) else NULL
    cbind(.ep09_group_header(z, by), data.frame(
      n_samples = nrow(z), valid_fraction = mean(valid_vec, na.rm = TRUE), data_loss = 1 - mean(valid_vec, na.rm = TRUE),
      effective_hz = ef$effective_hz[[1L]], sampling_interval_cv = ef$interval_cv[[1L]],
      rms_s2s = if (nrow(pr)) pr$rms_s2s[[1L]] else NA_real_,
      mean_radial_error = if (!is.null(acc) && nrow(acc)) acc$mean_radial_error[[1L]] else NA_real_,
      stringsAsFactors = FALSE
    ))
  })
  structure(list(
    table = .ep09_rbind_fill(rows), coordinate_units = "input_coordinate_units",
    caveat = paste(
      "Data-quality metrics should be reported with their operational definitions and acquisition context.",
      "Whether quality is sufficient depends on the scientific question and analysis resolution."
    )
  ), class = "eye_data_quality_profile")
}

#' Compact reporting table for eye-tracking data quality
#' @param x Data-quality profile.
#' @return An R object containing compact reporting table for eye-tracking data quality. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
data_quality_reporting_table <- function(x) {
  if (!inherits(x, "eye_data_quality_profile")) stop("x must be an eye_data_quality_profile.", call. = FALSE)
  x$table
}

#' @export
print.eye_calibration_error_model <- function(x, ...) {
  cat("eyeprocess empirical calibration-error model\n")
  cat("  observations:", x$n, "\n")
  cat("  mean error  :", paste(format(x$mean_error, digits = 3), collapse = ", "), "\n")
  invisible(x)
}

#' @export
print.eye_data_quality_profile <- function(x, ...) {
  cat("eyeprocess gaze data-quality profile\n")
  cat("  groups:", nrow(x$table), "\n")
  invisible(x)
}
