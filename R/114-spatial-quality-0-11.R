# Standardized spatial-quality metrics -----------------------------------------
# Vendor-neutral quality metrics for gaze accuracy, precision, sampling, and loss.

.ep_sq_df <- function(x, name = "data") {
  if (is.data.frame(x)) return(x)
  out <- tryCatch(as.data.frame(x, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(out)) stop(name, " must be coercible to a data frame.", call. = FALSE)
  out
}

.ep_sq_by <- function(by) {
  if (is.null(by)) character() else as.character(by)
}

.ep_sq_req <- function(d, cols, name = "data") {
  cols <- unique(cols[!is.na(cols) & nzchar(cols)])
  miss <- setdiff(cols, names(d))
  if (length(miss)) stop(name, " is missing required columns: ", paste(miss, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

.ep_sq_num <- function(x) suppressWarnings(as.numeric(x))

.ep_sq_split <- function(d, by = NULL) {
  by <- .ep_sq_by(by)
  if (!length(by)) return(list(all = seq_len(nrow(d))))
  .ep_sq_req(d, by)
  split(seq_len(nrow(d)), interaction(d[by], drop = TRUE, lex.order = TRUE))
}

.ep_sq_header <- function(z, by = NULL) {
  by <- .ep_sq_by(by)
  if (!length(by)) return(data.frame(.row = 1L)[, FALSE, drop = FALSE])
  z[1L, by, drop = FALSE]
}

.ep_sq_rbind <- function(rows) {
  if (!length(rows)) return(data.frame())
  all_names <- unique(unlist(lapply(rows, names), use.names = FALSE))
  rows <- lapply(rows, function(x) {
    miss <- setdiff(all_names, names(x)); for (nm in miss) x[[nm]] <- NA
    x[all_names]
  })
  do.call(rbind, rows)
}

.ep_sq_time_scale <- function(unit) {
  unit <- tolower(as.character(unit)[1L])
  scales <- c(s = 1, ms = 1e-3, us = 1e-6, ns = 1e-9)
  if (!unit %in% names(scales)) stop("time_unit must be one of s, ms, us, ns.", call. = FALSE)
  unname(scales[[unit]])
}

.ep_sq_linear_unit <- function(unit) switch(unit, degrees = "deg", pixels = "px", normalized = "normalized")
.ep_sq_area_unit <- function(unit) switch(unit, degrees = "deg^2", pixels = "px^2", normalized = "normalized^2")

.ep_sq_geom <- function(geometry, name) {
  value <- suppressWarnings(as.numeric(geometry[[name]])[1L])
  if (!length(value) || !is.finite(value) || value <= 0) stop("geometry[['", name, "']] must be a finite positive number.", call. = FALSE)
  value
}

.ep_sq_convert_xy <- function(x, y, unit, output_unit, geometry = NULL) {
  unit <- tolower(as.character(unit)[1L]); output_unit <- tolower(as.character(output_unit)[1L])
  allowed <- c("pixels", "normalized", "degrees")
  if (!unit %in% allowed || !output_unit %in% allowed) stop("unit and output_unit must be 'pixels', 'normalized', or 'degrees'.", call. = FALSE)
  x <- .ep_sq_num(x); y <- .ep_sq_num(y)
  if (identical(unit, output_unit)) return(list(x = x, y = y, unit = output_unit))
  if (is.null(geometry)) stop("geometry is required when output_unit differs from input unit.", call. = FALSE)
  wpx <- .ep_sq_geom(geometry, "screen_width_px"); hpx <- .ep_sq_geom(geometry, "screen_height_px")
  wcm <- .ep_sq_geom(geometry, "screen_width_cm"); hcm <- .ep_sq_geom(geometry, "screen_height_cm")
  dist <- .ep_sq_geom(geometry, "viewing_distance_cm")
  to_px <- function(a, axis) {
    if (unit == "pixels") return(a)
    if (unit == "normalized") {
      scale <- if (axis == "x") wpx else hpx
      return(a * scale)
    }
    cm <- tan(a * pi / 180) * dist
    scale <- if (axis == "x") wpx / wcm else hpx / hcm
    center <- if (axis == "x") wpx / 2 else hpx / 2
    cm * scale + center
  }
  px <- to_px(x, "x"); py <- to_px(y, "y")
  if (output_unit == "pixels") return(list(x = px, y = py, unit = output_unit))
  if (output_unit == "normalized") return(list(x = px / wpx, y = py / hpx, unit = output_unit))
  xcm <- (px - wpx / 2) * (wcm / wpx); ycm <- (py - hpx / 2) * (hcm / hpx)
  list(x = atan2(xcm, dist) * 180 / pi, y = atan2(ycm, dist) * 180 / pi, unit = output_unit)
}

.ep_sq_prepare <- function(d, x, y, target_x = NULL, target_y = NULL, unit = "degrees", output_unit = NULL, geometry = NULL) {
  output_unit <- if (is.null(output_unit)) unit else output_unit
  gaze <- .ep_sq_convert_xy(d[[x]], d[[y]], unit, output_unit, geometry)
  if (is.null(target_x) && is.null(target_y)) return(c(gaze, list(target_x = NULL, target_y = NULL)))
  if (is.null(target_x) || is.null(target_y)) stop("target_x and target_y must be supplied together.", call. = FALSE)
  target <- .ep_sq_convert_xy(d[[target_x]], d[[target_y]], unit, output_unit, geometry)
  list(x = gaze$x, y = gaze$y, unit = gaze$unit, target_x = target$x, target_y = target$y)
}

.ep_sq_fingerprint <- function(d, cols) {
  cols <- unique(cols[cols %in% names(d)])
  tf <- tempfile("eyeprocess-quality-", fileext = ".csv")
  on.exit(unlink(tf), add = TRUE)
  utils::write.csv(d[cols], tf, row.names = FALSE, na = "<NA>")
  unname(tools::md5sum(tf)[[1L]])
}

.ep_sq_provenance <- function(x, meta) {
  attr(x, "gaze_quality_provenance") <- meta
  x
}

#' Validate gaze-quality inputs
#' @export
validate_gaze_quality_inputs <- function(data, x = "gaze_x", y = "gaze_y", time = NULL,
                                         target_x = NULL, target_y = NULL, by = NULL,
                                         unit = "degrees", time_unit = "ms", unit_column = NULL) {
  d <- .ep_sq_df(data); by <- .ep_sq_by(by)
  .ep_sq_req(d, c(x, y, time, target_x, target_y, by, unit_column))
  if (xor(is.null(target_x), is.null(target_y))) stop("target_x and target_y must be supplied together.", call. = FALSE)
  unit <- tolower(as.character(unit)[1L]); if (!unit %in% c("pixels", "normalized", "degrees")) stop("unit must be 'pixels', 'normalized', or 'degrees'.", call. = FALSE)
  if (!is.null(time)) scale <- .ep_sq_time_scale(time_unit) else scale <- NA_real_
  if (!is.null(unit_column)) {
    vals <- unique(tolower(as.character(stats::na.omit(d[[unit_column]]))))
    if (length(vals) > 1L) stop("mixed coordinate units are not allowed within one call.", call. = FALSE)
    if (length(vals) && vals[[1L]] != unit) stop("unit conflicts with unit_column.", call. = FALSE)
  }
  issues <- list()
  if (!is.null(time)) {
    for (idx in .ep_sq_split(d, by)) {
      z <- d[idx, , drop = FALSE]; tt <- .ep_sq_num(z[[time]]) * scale; tt <- tt[is.finite(tt)]
      dt <- if (length(tt) > 1L) diff(tt) else numeric(); flags <- character()
      if (any(dt < 0)) flags <- c(flags, "non_monotonic_timestamps")
      if (any(dt == 0)) flags <- c(flags, "duplicate_timestamps")
      if (length(flags)) issues[[length(issues) + 1L]] <- cbind(.ep_sq_header(z, by), data.frame(issues = paste(flags, collapse = ";"), stringsAsFactors = FALSE))
    }
  }
  list(n_rows = nrow(d), coordinate_unit = unit, time_unit = if (is.null(time)) NULL else time_unit,
       group_issues = .ep_sq_rbind(issues), valid = TRUE)
}

#' Compute target-referenced gaze accuracy
#' @export
compute_gaze_accuracy <- function(data, x = "gaze_x", y = "gaze_y", target_x = "target_x", target_y = "target_y",
                                  by = NULL, unit = "degrees", output_unit = NULL, geometry = NULL) {
  d <- .ep_sq_df(data); by <- .ep_sq_by(by); .ep_sq_req(d, c(x, y, target_x, target_y, by))
  rows <- lapply(.ep_sq_split(d, by), function(idx) {
    z <- d[idx, , drop = FALSE]; p <- .ep_sq_prepare(z, x, y, target_x, target_y, unit, output_unit, geometry)
    ok <- is.finite(p$x) & is.finite(p$y) & is.finite(p$target_x) & is.finite(p$target_y)
    dx <- p$x[ok] - p$target_x[ok]; dy <- p$y[ok] - p$target_y[ok]; radial <- sqrt(dx^2 + dy^2)
    nt <- if (any(ok)) nrow(unique(data.frame(x = p$target_x[ok], y = p$target_y[ok]))) else 0L
    cbind(.ep_sq_header(z, by), data.frame(n_accuracy_samples = sum(ok), n_accuracy_targets = nt,
      accuracy_mean = if (length(radial)) mean(radial) else NA_real_, accuracy_median = if (length(radial)) stats::median(radial) else NA_real_,
      accuracy_horizontal = if (length(dx)) mean(abs(dx)) else NA_real_, accuracy_vertical = if (length(dy)) mean(abs(dy)) else NA_real_,
      accuracy_euclidean = if (length(radial)) mean(radial) else NA_real_, accuracy_bias_x = if (length(dx)) mean(dx) else NA_real_,
      accuracy_bias_y = if (length(dy)) mean(dy) else NA_real_, unit = .ep_sq_linear_unit(p$unit), stringsAsFactors = FALSE))
  })
  .ep_sq_provenance(.ep_sq_rbind(rows), list(function_name = "compute_gaze_accuracy", input_unit = unit, output_unit = if (is.null(output_unit)) unit else output_unit))
}

#' Compute RMS sample-to-sample gaze precision
#' @export
compute_rms_s2s <- function(data, x = "gaze_x", y = "gaze_y", time = NULL, by = NULL,
                            unit = "degrees", output_unit = NULL, geometry = NULL,
                            dimension = c("2d", "horizontal", "vertical"), time_unit = "ms", max_gap_ms = NULL) {
  dimension <- match.arg(dimension); d <- .ep_sq_df(data); by <- .ep_sq_by(by); .ep_sq_req(d, c(x, y, time, by))
  if (!is.null(max_gap_ms) && (!is.finite(max_gap_ms) || max_gap_ms <= 0)) stop("max_gap_ms must be a finite positive value when supplied.", call. = FALSE)
  if (!is.null(max_gap_ms) && is.null(time)) stop("time is required when max_gap_ms is supplied.", call. = FALSE)
  scale <- if (is.null(time)) NA_real_ else .ep_sq_time_scale(time_unit)
  rows <- lapply(.ep_sq_split(d, by), function(idx) {
    z <- d[idx, , drop = FALSE]; p <- .ep_sq_prepare(z, x, y, unit = unit, output_unit = output_unit, geometry = geometry)
    n <- nrow(z); finite_pair <- if (n > 1L) is.finite(p$x[-n]) & is.finite(p$y[-n]) & is.finite(p$x[-1L]) & is.finite(p$y[-1L]) else logical()
    dx <- diff(p$x); dy <- diff(p$y); step <- switch(dimension, horizontal = abs(dx), vertical = abs(dy), `2d` = sqrt(dx^2 + dy^2))
    keep <- finite_pair & is.finite(step)
    if (!is.null(time) && n > 1L) {
      dt_ms <- diff(.ep_sq_num(z[[time]]) * scale) * 1000; keep <- keep & is.finite(dt_ms) & dt_ms > 0
      if (!is.null(max_gap_ms)) keep <- keep & dt_ms <= max_gap_ms
    }
    vals <- step[keep]
    cbind(.ep_sq_header(z, by), data.frame(n_steps = length(vals), precision_rms_s2s = if (length(vals)) sqrt(mean(vals^2)) else NA_real_,
      median_s2s = if (length(vals)) stats::median(vals) else NA_real_, p95_s2s = if (length(vals)) unname(stats::quantile(vals, .95, names = FALSE)) else NA_real_,
      dimension = dimension, unit = .ep_sq_linear_unit(p$unit), max_gap_ms = if (is.null(max_gap_ms)) NA_real_ else max_gap_ms, stringsAsFactors = FALSE))
  })
  .ep_sq_provenance(.ep_sq_rbind(rows), list(function_name = "compute_rms_s2s", missing_gap_policy = "never_bridge", max_gap_ms = max_gap_ms))
}

#' Compute standard-deviation gaze precision
#' @export
compute_gaze_sd_precision <- function(data, x = "gaze_x", y = "gaze_y", by = NULL,
                                      unit = "degrees", output_unit = NULL, geometry = NULL) {
  d <- .ep_sq_df(data); by <- .ep_sq_by(by); .ep_sq_req(d, c(x, y, by))
  rows <- lapply(.ep_sq_split(d, by), function(idx) {
    z <- d[idx, , drop = FALSE]; p <- .ep_sq_prepare(z, x, y, unit = unit, output_unit = output_unit, geometry = geometry)
    ok <- is.finite(p$x) & is.finite(p$y); xx <- p$x[ok]; yy <- p$y[ok]
    sx <- if (length(xx)) sqrt(mean((xx - mean(xx))^2)) else NA_real_; sy <- if (length(yy)) sqrt(mean((yy - mean(yy))^2)) else NA_real_
    cbind(.ep_sq_header(z, by), data.frame(n_precision_samples = sum(ok), precision_sd_x = sx, precision_sd_y = sy,
      precision_sd = if (is.finite(sx) && is.finite(sy)) sqrt(sx^2 + sy^2) else NA_real_, unit = .ep_sq_linear_unit(p$unit), stringsAsFactors = FALSE))
  })
  .ep_sq_provenance(.ep_sq_rbind(rows), list(function_name = "compute_gaze_sd_precision"))
}

#' Compute bivariate contour ellipse area
#' @export
compute_bcea <- function(data, x = "gaze_x", y = "gaze_y", by = NULL, probability = .68,
                         unit = "degrees", output_unit = NULL, geometry = NULL) {
  probability <- as.numeric(probability)[1L]
  if (!is.finite(probability) || probability <= 0 || probability >= 1) stop("probability must lie strictly between 0 and 1.", call. = FALSE)
  d <- .ep_sq_df(data); by <- .ep_sq_by(by); .ep_sq_req(d, c(x, y, by)); k <- -log(1 - probability)
  rows <- lapply(.ep_sq_split(d, by), function(idx) {
    z <- d[idx, , drop = FALSE]; p <- .ep_sq_prepare(z, x, y, unit = unit, output_unit = output_unit, geometry = geometry)
    ok <- is.finite(p$x) & is.finite(p$y); xx <- p$x[ok]; yy <- p$y[ok]
    if (length(xx) > 1L) {
      sx <- sqrt(mean((xx - mean(xx))^2)); sy <- sqrt(mean((yy - mean(yy))^2)); rho <- if (sx > 0 && sy > 0) stats::cor(xx, yy) else 0
      rho <- max(-1, min(1, rho)); area <- 2 * pi * k * sx * sy * sqrt(max(0, 1 - rho^2))
    } else sx <- sy <- rho <- area <- NA_real_
    cbind(.ep_sq_header(z, by), data.frame(n_bcea_samples = sum(ok), bcea = area, bcea_probability = probability,
      sd_x = sx, sd_y = sy, correlation_xy = rho, unit = .ep_sq_area_unit(p$unit), stringsAsFactors = FALSE))
  })
  .ep_sq_provenance(.ep_sq_rbind(rows), list(function_name = "compute_bcea", probability = probability))
}

#' Compute complementary gaze precision metrics
#' @export
compute_gaze_precision <- function(data, x = "gaze_x", y = "gaze_y", time = NULL, by = NULL,
                                   probability = .68, unit = "degrees", output_unit = NULL, geometry = NULL,
                                   dimension = "2d", time_unit = "ms", max_gap_ms = NULL) {
  list(rms_s2s = compute_rms_s2s(data, x, y, time, by, unit, output_unit, geometry, dimension, time_unit, max_gap_ms),
       sd = compute_gaze_sd_precision(data, x, y, by, unit, output_unit, geometry),
       bcea = compute_bcea(data, x, y, by, probability, unit, output_unit, geometry))
}

#' Estimate sampling intervals
#' @export
estimate_sampling_interval <- function(data, time = "timestamp_ms", by = NULL, time_unit = "ms") {
  d <- .ep_sq_df(data); by <- .ep_sq_by(by); .ep_sq_req(d, c(time, by)); scale <- .ep_sq_time_scale(time_unit)
  rows <- lapply(.ep_sq_split(d, by), function(idx) {
    z <- d[idx, , drop = FALSE]; tt <- .ep_sq_num(z[[time]]) * scale; tt <- tt[is.finite(tt)]
    dt <- if (length(tt) > 1L) diff(tt) * 1000 else numeric(); pos <- dt[dt > 0 & is.finite(dt)]
    cbind(.ep_sq_header(z, by), data.frame(n_observed_timestamps = length(tt), n_intervals = length(pos),
      median_interval_ms = if (length(pos)) stats::median(pos) else NA_real_, mean_interval_ms = if (length(pos)) mean(pos) else NA_real_,
      min_interval_ms = if (length(pos)) min(pos) else NA_real_, max_interval_ms = if (length(pos)) max(pos) else NA_real_,
      duplicate_timestamp_count = sum(dt == 0, na.rm = TRUE), non_monotonic_timestamp_count = sum(dt < 0, na.rm = TRUE), stringsAsFactors = FALSE))
  })
  .ep_sq_provenance(.ep_sq_rbind(rows), list(function_name = "estimate_sampling_interval", time_unit = time_unit))
}

#' Estimate sampling jitter
#' @export
estimate_sampling_jitter <- function(data, time = "timestamp_ms", by = NULL, time_unit = "ms") {
  d <- .ep_sq_df(data); by <- .ep_sq_by(by); .ep_sq_req(d, c(time, by)); scale <- .ep_sq_time_scale(time_unit)
  rows <- lapply(.ep_sq_split(d, by), function(idx) {
    z <- d[idx, , drop = FALSE]; tt <- .ep_sq_num(z[[time]]) * scale; tt <- tt[is.finite(tt)]
    dt <- if (length(tt) > 1L) diff(tt) * 1000 else numeric(); pos <- dt[dt > 0 & is.finite(dt)]; med <- if (length(pos)) stats::median(pos) else NA_real_
    cbind(.ep_sq_header(z, by), data.frame(sampling_jitter_ms = if (length(pos) > 1L) stats::sd(pos - med) else NA_real_,
      sampling_jitter_mad_ms = if (length(pos)) stats::median(abs(pos - med)) else NA_real_, median_interval_ms = med, stringsAsFactors = FALSE))
  })
  .ep_sq_provenance(.ep_sq_rbind(rows), list(function_name = "estimate_sampling_jitter", definition = "SD of positive inter-sample intervals around their median"))
}

#' Estimate effective sampling rate
#' @export
estimate_effective_sampling_rate <- function(data, time = "timestamp_ms", by = NULL, time_unit = "ms",
                                             nominal_sampling_hz = NULL, dropped_interval_factor = 1.5,
                                             x = NULL, y = NULL, valid = NULL) {
  d <- .ep_sq_df(data); by <- .ep_sq_by(by); .ep_sq_req(d, c(time, x, y, valid, by)); scale <- .ep_sq_time_scale(time_unit)
  if (xor(is.null(x), is.null(y))) stop("x and y must either both be supplied or both be omitted.", call. = FALSE)
  if (!is.null(valid) && is.null(x)) stop("x and y are required when valid is supplied.", call. = FALSE)
  if (!is.null(nominal_sampling_hz) && (!is.finite(nominal_sampling_hz) || nominal_sampling_hz <= 0)) stop("nominal_sampling_hz must be positive.", call. = FALSE)
  if (!is.finite(dropped_interval_factor) || dropped_interval_factor <= 1) stop("dropped_interval_factor must be > 1.", call. = FALSE)
  rows <- lapply(.ep_sq_split(d, by), function(idx) {
    z <- d[idx, , drop = FALSE]; tall <- .ep_sq_num(z[[time]]) * scale; finite_t <- is.finite(tall); tt <- tall[finite_t]
    dt <- if (length(tt) > 1L) diff(tt) else numeric(); pos <- dt[dt > 0 & is.finite(dt)]; med <- if (length(pos)) stats::median(pos) else NA_real_
    span <- if (length(tt) > 1L) tail(tt, 1) - head(tt, 1) else NA_real_
    duration <- if (is.finite(span) && is.finite(med) && med > 0) span + med else if (length(tt) == 1L && !is.null(nominal_sampling_hz)) 1 / nominal_sampling_hz else NA_real_
    if (!is.null(x)) {
      m <- .ep_sq_valid_masks(z, x, y, valid); effective_count <- sum(m$good & finite_t); count_rule <- "valid gaze samples with finite timestamps"
    } else { effective_count <- length(tt); count_rule <- "finite timestamps" }
    hz <- if (is.finite(duration) && duration > 0) effective_count / duration else NA_real_
    dropped <- if (!is.null(nominal_sampling_hz) && length(pos)) sum(pos > dropped_interval_factor / nominal_sampling_hz) else NA_integer_
    cbind(.ep_sq_header(z, by), data.frame(observed_sample_count = length(tt), effective_sample_count = effective_count, timestamp_span_s = span,
      trial_duration_s = duration, effective_sampling_hz = hz, median_interval_ms = if (is.finite(med)) med * 1000 else NA_real_,
      dropped_interval_count = dropped, nominal_sampling_hz = if (is.null(nominal_sampling_hz)) NA_real_ else nominal_sampling_hz,
      effective_count_rule = count_rule, stringsAsFactors = FALSE))
  })
  .ep_sq_provenance(.ep_sq_rbind(rows), list(function_name = "estimate_effective_sampling_rate",
    definition = "effective sample count / estimated recording duration", duration_estimator = "timestamp span plus one median positive inter-sample interval",
    dropped_interval_factor = dropped_interval_factor))
}

.ep_sq_valid_masks <- function(d, x, y, valid = NULL) {
  gx <- .ep_sq_num(d[[x]]); gy <- .ep_sq_num(d[[y]]); missing <- !is.finite(gx) | !is.finite(gy)
  if (is.null(valid)) explicit <- rep(TRUE, nrow(d)) else if (is.logical(d[[valid]])) explicit <- !is.na(d[[valid]]) & d[[valid]] else {
    v <- .ep_sq_num(d[[valid]]); explicit <- is.finite(v) & v > 0
  }
  list(good = !missing & explicit, invalid = !missing & !explicit, missing = missing)
}

#' Compute valid-sample fraction
#' @export
compute_valid_sample_fraction <- function(data, x = "gaze_x", y = "gaze_y", valid = NULL, by = NULL) {
  d <- .ep_sq_df(data); by <- .ep_sq_by(by); .ep_sq_req(d, c(x, y, valid, by))
  rows <- lapply(.ep_sq_split(d, by), function(idx) {
    z <- d[idx, , drop = FALSE]; m <- .ep_sq_valid_masks(z, x, y, valid); n <- nrow(z)
    cbind(.ep_sq_header(z, by), data.frame(n_samples = n, valid_sample_fraction = if (n) mean(m$good) else NA_real_,
      invalid_sample_fraction = if (n) mean(m$invalid) else NA_real_, missing_sample_fraction = if (n) mean(m$missing) else NA_real_, stringsAsFactors = FALSE))
  })
  .ep_sq_provenance(.ep_sq_rbind(rows), list(function_name = "compute_valid_sample_fraction", validity_rule = if (is.null(valid)) "finite x and y" else valid))
}

#' Compute gaze data loss and missing runs
#' @export
compute_gaze_data_loss <- function(data, x = "gaze_x", y = "gaze_y", time = "timestamp_ms", valid = NULL,
                                   missing_reason = NULL, by = NULL, time_unit = "ms") {
  d <- .ep_sq_df(data); by <- .ep_sq_by(by); .ep_sq_req(d, c(x, y, time, valid, missing_reason, by)); scale <- if (is.null(time)) NA_real_ else .ep_sq_time_scale(time_unit)
  rows <- lapply(.ep_sq_split(d, by), function(idx) {
    z <- d[idx, , drop = FALSE]; m <- .ep_sq_valid_masks(z, x, y, valid); lost <- !m$good; n <- nrow(z)
    rr <- rle(lost); ends <- cumsum(rr$lengths); starts <- ends - rr$lengths + 1L; take <- which(rr$values)
    runs <- if (length(take)) data.frame(start = starts[take], end = ends[take]) else data.frame(start = integer(), end = integer())
    longest_samples <- if (nrow(runs)) max(runs$end - runs$start + 1L) else 0L; longest_ms <- NA_real_
    if (!is.null(time) && nrow(runs)) {
      tt <- .ep_sq_num(z[[time]]) * scale * 1000; pos <- diff(tt); pos <- pos[is.finite(pos) & pos > 0]; typical <- if (length(pos)) stats::median(pos) else 0
      durations <- vapply(seq_len(nrow(runs)), function(i) {
        a <- runs$start[i]; b <- runs$end[i]; if (is.finite(tt[a]) && is.finite(tt[b])) max(0, tt[b] - tt[a]) + typical else NA_real_
      }, numeric(1)); if (any(is.finite(durations))) longest_ms <- max(durations, na.rm = TRUE)
    }
    row <- cbind(.ep_sq_header(z, by), data.frame(n_samples = n, valid_sample_fraction = if (n) mean(m$good) else NA_real_,
      invalid_sample_fraction = if (n) mean(m$invalid) else NA_real_, missing_sample_fraction = if (n) mean(m$missing) else NA_real_,
      data_loss_fraction = if (n) mean(lost) else NA_real_, missing_run_count = nrow(runs), longest_missing_run_samples = longest_samples,
      longest_missing_run_ms = longest_ms, stringsAsFactors = FALSE))
    if (!is.null(missing_reason)) {
      reason <- tolower(as.character(z[[missing_reason]][lost])); reason[is.na(reason) | !nzchar(reason)] <- "unknown"; tab <- table(reason)
      for (nm in names(tab)) row[[paste0("missing_reason_", nm, "_fraction")]] <- as.numeric(tab[[nm]]) / max(1, sum(lost))
    }
    row
  })
  .ep_sq_provenance(.ep_sq_rbind(rows), list(function_name = "compute_gaze_data_loss", loss_rule = "missing coordinates or explicit invalidity"))
}

.ep_sq_merge <- function(parts, by) {
  by <- .ep_sq_by(by)
  if (!length(parts)) return(data.frame())
  if (!length(by)) {
    out <- do.call(cbind, lapply(parts, function(x) x[setdiff(names(x), ".group"), drop = FALSE])); return(out[!duplicated(names(out))])
  }
  Reduce(function(a, b) merge(a, b, by = by, all = TRUE, sort = FALSE, suffixes = c("", ".dup")), parts)
}

#' Summarise spatial gaze quality
#' @export
summarise_spatial_quality <- function(data, x = "gaze_x", y = "gaze_y", target_x = "target_x", target_y = "target_y",
                                      time = NULL, by = NULL, unit = "degrees", output_unit = NULL, geometry = NULL,
                                      dimension = "2d", time_unit = "ms", max_gap_ms = NULL, probability = .68) {
  .ep_sq_provenance(.ep_sq_merge(list(
    compute_gaze_accuracy(data, x, y, target_x, target_y, by, unit, output_unit, geometry),
    compute_rms_s2s(data, x, y, time, by, unit, output_unit, geometry, dimension, time_unit, max_gap_ms),
    compute_gaze_sd_precision(data, x, y, by, unit, output_unit, geometry),
    compute_bcea(data, x, y, by, probability, unit, output_unit, geometry)), by), list(function_name = "summarise_spatial_quality"))
}

#' Summarise sampling quality
#' @export
summarise_sampling_quality <- function(data, time = "timestamp_ms", by = NULL, time_unit = "ms", nominal_sampling_hz = NULL,
                                       dropped_interval_factor = 1.5, x = NULL, y = NULL, valid = NULL) {
  .ep_sq_provenance(.ep_sq_merge(list(estimate_sampling_interval(data, time, by, time_unit), estimate_sampling_jitter(data, time, by, time_unit),
    estimate_effective_sampling_rate(data, time, by, time_unit, nominal_sampling_hz, dropped_interval_factor, x, y, valid)), by), list(function_name = "summarise_sampling_quality"))
}

.ep_sq_threshold_flags <- function(row, thresholds) {
  if (is.null(thresholds) || !length(thresholds)) return(character())
  flags <- character()
  for (metric in names(thresholds)) {
    if (!metric %in% names(row) || !is.finite(suppressWarnings(as.numeric(row[[metric]])[1L]))) next
    value <- as.numeric(row[[metric]])[1L]; rule <- thresholds[[metric]]
    if (is.list(rule)) {
      if (!is.null(rule$max) && value > rule$max) flags <- c(flags, paste0(metric, ">max"))
      if (!is.null(rule$min) && value < rule$min) flags <- c(flags, paste0(metric, "<min"))
    } else if (value > as.numeric(rule)[1L]) flags <- c(flags, paste0(metric, ">max"))
  }
  unique(flags)
}

#' Create a canonical gaze-quality report
#' @export
create_gaze_quality_report <- function(data, x = "gaze_x", y = "gaze_y", time = "timestamp_ms", target_x = "target_x", target_y = "target_y",
                                       valid = NULL, missing_reason = NULL, by = NULL, unit = "degrees", output_unit = NULL, geometry = NULL,
                                       time_unit = "ms", nominal_sampling_hz = NULL, bcea_probability = .68, max_gap_ms = NULL, thresholds = NULL,
                                       preprocessing_spec = NULL, event_detector = NULL, aoi_specification = NULL, quality_rules = NULL,
                                       model_specification = NULL, software_version = NULL) {
  d <- .ep_sq_df(data); by <- .ep_sq_by(by); .ep_sq_req(d, c(x, y, time, valid, missing_reason, by))
  has_targets <- !is.null(target_x) && !is.null(target_y) && target_x %in% names(d) && target_y %in% names(d)
  validation <- validate_gaze_quality_inputs(d, x, y, time, if (has_targets) target_x else NULL, if (has_targets) target_y else NULL, by, unit, time_unit)
  spatial <- if (has_targets) summarise_spatial_quality(d, x, y, target_x, target_y, time, by, unit, output_unit, geometry, "2d", time_unit, max_gap_ms, bcea_probability) else
    .ep_sq_merge(list(compute_rms_s2s(d, x, y, time, by, unit, output_unit, geometry, "2d", time_unit, max_gap_ms),
                      compute_gaze_sd_precision(d, x, y, by, unit, output_unit, geometry), compute_bcea(d, x, y, by, bcea_probability, unit, output_unit, geometry)), by)
  report <- .ep_sq_merge(list(spatial, summarise_sampling_quality(d, time, by, time_unit, nominal_sampling_hz, 1.5, x, y, valid),
                              compute_gaze_data_loss(d, x, y, time, valid, missing_reason, by, time_unit)), by)
  report$quality_flags <- character(nrow(report)); report$review_required <- FALSE
  for (i in seq_len(nrow(report))) {
    flags <- character()
    if (has_targets && "n_accuracy_targets" %in% names(report) && is.finite(report$n_accuracy_targets[i]) && report$n_accuracy_targets[i] > 1) flags <- c(flags, "mixed_accuracy_targets")
    if (nrow(validation$group_issues)) {
      if (!length(by)) flags <- c(flags, unlist(strsplit(validation$group_issues$issues, ";", fixed = TRUE))) else {
        hit <- rep(TRUE, nrow(validation$group_issues)); for (nm in by) hit <- hit & as.character(validation$group_issues[[nm]]) == as.character(report[[nm]][i])
        if (any(hit)) flags <- c(flags, unlist(strsplit(validation$group_issues$issues[hit], ";", fixed = TRUE)))
      }
    }
    flags <- unique(c(flags, .ep_sq_threshold_flags(report[i, , drop = FALSE], thresholds)))
    report$quality_flags[i] <- paste(flags, collapse = ";"); report$review_required[i] <- length(flags) > 0
  }
  prov <- list(source_fingerprint = .ep_sq_fingerprint(d, c(x, y, time, if (has_targets) c(target_x, target_y), by)), preprocessing_spec = preprocessing_spec,
               event_detector = event_detector, aoi_specification = aoi_specification, quality_rules = if (is.null(quality_rules)) thresholds else quality_rules,
               model_specification = model_specification, software_version = software_version, input_unit = unit, output_unit = if (is.null(output_unit)) unit else output_unit,
               time_unit = time_unit, nominal_sampling_hz = nominal_sampling_hz, bcea_probability = bcea_probability, max_gap_ms = max_gap_ms, automatic_exclusion = FALSE)
  class(report) <- c("gaze_quality_report", class(report)); .ep_sq_provenance(report, prov)
}

#' Compare gaze quality across sessions
#' @export
compare_gaze_quality_sessions <- function(data, session = "session_id", by = NULL, ...) {
  d <- .ep_sq_df(data); .ep_sq_req(d, session); create_gaze_quality_report(d, by = unique(c(.ep_sq_by(by), session)), ...)
}

#' Compare gaze quality across conditions
#' @export
compare_gaze_quality_conditions <- function(data, condition = "condition", by = NULL, ...) {
  d <- .ep_sq_df(data); .ep_sq_req(d, condition); create_gaze_quality_report(d, by = unique(c(.ep_sq_by(by), condition)), ...)
}

#' Plot gaze accuracy
#' @export
plot_gaze_accuracy <- function(report, metric = "accuracy_mean", ...) {
  d <- .ep_sq_df(report); .ep_sq_req(d, metric); graphics::plot(seq_len(nrow(d)), d[[metric]], type = "b", xlab = "analysis unit", ylab = metric, main = "Gaze accuracy", ...); invisible(d)
}

#' Plot gaze precision
#' @export
plot_gaze_precision <- function(report, metric = "precision_rms_s2s", ...) {
  d <- .ep_sq_df(report); .ep_sq_req(d, metric); graphics::plot(seq_len(nrow(d)), d[[metric]], type = "b", xlab = "analysis unit", ylab = metric, main = "Gaze precision", ...); invisible(d)
}

#' Plot BCEA
#' @export
plot_bcea <- function(report, ...) {
  d <- .ep_sq_df(report); .ep_sq_req(d, "bcea"); graphics::barplot(d$bcea, xlab = "analysis unit", ylab = "BCEA", main = "Bivariate contour ellipse area", ...); invisible(d)
}

#' Plot sampling intervals
#' @export
plot_sampling_intervals <- function(data, time = "timestamp_ms", time_unit = "ms", ...) {
  d <- .ep_sq_df(data); .ep_sq_req(d, time); tt <- .ep_sq_num(d[[time]]) * .ep_sq_time_scale(time_unit); dt <- diff(tt) * 1000
  graphics::plot(seq_along(dt), dt, type = "b", xlab = "interval", ylab = "interval (ms)", main = "Sampling intervals", ...); invisible(data.frame(interval_ms = dt))
}

#' Plot gaze-quality dashboard
#' @export
plot_gaze_quality_dashboard <- function(report, ...) {
  d <- .ep_sq_df(report); old <- graphics::par(mfrow = c(2, 2)); on.exit(graphics::par(old), add = TRUE)
  for (metric in c("accuracy_mean", "precision_rms_s2s", "bcea", "valid_sample_fraction")) {
    if (metric %in% names(d)) graphics::barplot(d[[metric]], main = metric, ...) else graphics::plot.new()
  }
  invisible(d)
}

#' Create manuscript-ready gaze-quality reporting text
#' @export
report_gaze_quality <- function(report, digits = 3L) {
  d <- .ep_sq_df(report); if (!nrow(d)) return("No gaze-quality rows were available.")
  metrics <- intersect(c("accuracy_mean", "precision_rms_s2s", "precision_sd", "bcea", "effective_sampling_hz", "valid_sample_fraction", "data_loss_fraction"), names(d))
  parts <- vapply(metrics, function(metric) {
    v <- .ep_sq_num(d[[metric]]); v <- v[is.finite(v)]; if (!length(v)) return(NA_character_)
    paste0(metric, ": mean ", format(round(mean(v), digits), nsmall = digits), ", range ", format(round(min(v), digits), nsmall = digits), "–", format(round(max(v), digits), nsmall = digits))
  }, character(1)); parts <- parts[!is.na(parts)]
  n_review <- if ("review_required" %in% names(d)) sum(as.logical(d$review_required), na.rm = TRUE) else 0L
  paste0(paste(parts, collapse = "; "), ". Review required for ", n_review, "/", nrow(d), " analysis units. Thresholds, when supplied, are study-specific review rules and never trigger automatic exclusion.")
}

#' Simulate a nine-point gaze-quality validation dataset
#' @export
simulate_gaze_quality_calibration <- function(seed = 20260918L, samples_per_target = 18L, nominal_sampling_hz = 60) {
  samples_per_target <- as.integer(samples_per_target); if (samples_per_target < 4L) stop("samples_per_target must be at least 4.", call. = FALSE)
  set.seed(as.integer(seed)); targets <- expand.grid(target_x = c(-5, 0, 5), target_y = c(-5, 0, 5))
  specs <- data.frame(profile = c("good_accuracy_good_precision", "poor_accuracy_good_precision", "good_accuracy_poor_precision", "poor_accuracy_poor_precision", "irregular_sampling", "missingness"),
                      bias_x = c(0, .9, 0, .9, .1, .1), bias_y = c(0, -.7, 0, -.7, -.1, -.1), sd = c(.12, .12, .75, .75, .20, .20), stringsAsFactors = FALSE)
  rows <- list(); k <- 0L
  for (s in seq_len(nrow(specs))) {
    t <- 0
    for (tid in seq_len(nrow(targets))) for (sample in seq_len(samples_per_target)) {
      dt <- 1 / nominal_sampling_hz
      if (specs$profile[s] == "irregular_sampling") { dt <- dt * max(.25, 1 + stats::rnorm(1, 0, .35)); if (sample %in% c(7L, 14L)) dt <- dt * 2.5 }
      t <- t + dt; gx <- targets$target_x[tid] + specs$bias_x[s] + stats::rnorm(1, 0, specs$sd[s]); gy <- targets$target_y[tid] + specs$bias_y[s] + stats::rnorm(1, 0, specs$sd[s])
      valid <- TRUE; reason <- NA_character_
      if (specs$profile[s] == "missingness" && sample %in% c(6L, 7L, 8L, 15L)) { gx <- gy <- NA_real_; valid <- FALSE; reason <- if (sample %in% c(6L, 7L)) "blink" else "tracker_invalidity" }
      k <- k + 1L; rows[[k]] <- data.frame(participant_id = "P001", session_id = "S001", profile = specs$profile[s], target_id = tid,
        sample_in_target = sample, timestamp_ms = t * 1000, target_x = targets$target_x[tid], target_y = targets$target_y[tid], gaze_x = gx, gaze_y = gy,
        valid = valid, missing_reason = reason, coordinate_unit = "degrees", stringsAsFactors = FALSE)
    }
  }
  .ep_sq_rbind(rows)
}
