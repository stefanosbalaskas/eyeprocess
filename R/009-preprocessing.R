preprocess_spec <- function(
    gaze_filter = "none",
    gaze_window = 5L,
    pupil_interpolation = "linear",
    pupil_max_gap_ms = 150,
    pupil_filter = "median",
    pupil_window = 5L,
    pupil_baseline = "subtract",
    pupil_baseline_window = c(-0.2, 0),
    fixation_algorithm = "none",
    fixation_parameters = list(),
    blink_detection = TRUE,
    exclusions = list()) {
  structure(list(
    gaze_filter = gaze_filter,
    gaze_window = as.integer(gaze_window),
    pupil_interpolation = pupil_interpolation,
    pupil_max_gap_ms = as.numeric(pupil_max_gap_ms),
    pupil_filter = pupil_filter,
    pupil_window = as.integer(pupil_window),
    pupil_baseline = pupil_baseline,
    pupil_baseline_window = as.numeric(pupil_baseline_window),
    fixation_algorithm = fixation_algorithm,
    fixation_parameters = fixation_parameters,
    blink_detection = isTRUE(blink_detection),
    exclusions = exclusions
  ), class = "eye_preprocess_spec")
}

print.eye_preprocess_spec <- function(x, ...) {
  cat("<eye_preprocess_spec>\n")
  cat("  Gaze filter:       ", x$gaze_filter, "\n", sep = "")
  cat("  Pupil interpolation:", x$pupil_interpolation, " (max gap ", x$pupil_max_gap_ms, " ms)\n", sep = "")
  cat("  Pupil filter:      ", x$pupil_filter, "\n", sep = "")
  cat("  Fixation algorithm:", x$fixation_algorithm, "\n", sep = "")
  invisible(x)
}

rolling_apply <- function(x, width = 5L, FUN = stats::median, na.rm = TRUE) {
  width <- as.integer(width)
  if (width < 1L) .eye_stop("`width` must be positive.")
  if (width %% 2L == 0L) width <- width + 1L
  n <- length(x); half <- floor(width / 2)
  out <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    idx <- max(1L, i - half):min(n, i + half)
    out[i] <- FUN(x[idx], na.rm = na.rm)
  }
  out
}

filter_gaze <- function(x, method = c("median", "mean", "moving_median", "moving_average", "none"), window = 5L, component = "gaze_samples") {
  .assert_eye_dataset(x)
  method <- match.arg(method)
  if (method == "moving_median") method <- "median"
  if (method == "moving_average") method <- "mean"
  if (method == "none" || !nrow(x[[component]])) return(x)
  d <- x[[component]]
  .assert_columns(d, c("recording_id", "timestamp_seconds", "gaze_x", "gaze_y"), component)
  groups <- split(seq_len(nrow(d)), d$recording_id)
  fun <- if (method == "median") stats::median else mean
  for (idx in groups) {
    ord <- idx[order(d$timestamp_seconds[idx])]
    d$gaze_x[ord] <- rolling_apply(d$gaze_x[ord], window, fun)
    d$gaze_y[ord] <- rolling_apply(d$gaze_y[ord], window, fun)
  }
  x[[component]] <- d
  add_provenance(x, "filter_gaze", component, paste0("method=", method, ";window=", window), reversible = FALSE)
}

flag_gaze_outliers <- function(x, method = c("mad", "velocity", "bounds"), threshold = 6, max_velocity = NULL) {
  .assert_eye_dataset(x)
  method <- match.arg(method)
  d <- x$gaze_samples
  if (!nrow(d)) return(x)
  flag <- rep(FALSE, nrow(d))
  if (method == "mad") {
    groups <- split(seq_len(nrow(d)), d$recording_id)
    for (idx in groups) {
      mx <- stats::median(d$gaze_x[idx], na.rm = TRUE); my <- stats::median(d$gaze_y[idx], na.rm = TRUE)
      sx <- stats::mad(d$gaze_x[idx], na.rm = TRUE); sy <- stats::mad(d$gaze_y[idx], na.rm = TRUE)
      flag[idx] <- abs(d$gaze_x[idx] - mx) > threshold * sx | abs(d$gaze_y[idx] - my) > threshold * sy
    }
  } else if (method == "velocity") {
    if (is.null(max_velocity)) max_velocity <- threshold
    vel <- gaze_velocity(d)
    flag[match(vel$sample_id, d$sample_id)] <- vel$velocity > max_velocity
  } else {
    spaces <- x$coordinate_spaces
    for (id in unique(d$coordinate_space_id)) {
      idx <- which(d$coordinate_space_id == id)
      row <- spaces[spaces$coordinate_space_id == id, ]
      if (!nrow(row)) next
      if (row$x_unit[1L] == "normalized") flag[idx] <- d$gaze_x[idx] < 0 | d$gaze_x[idx] > 1 | d$gaze_y[idx] < 0 | d$gaze_y[idx] > 1
      if (row$x_unit[1L] == "pixels" && is.finite(row$width[1L]) && is.finite(row$height[1L])) flag[idx] <- d$gaze_x[idx] < 0 | d$gaze_x[idx] > row$width[1L] | d$gaze_y[idx] < 0 | d$gaze_y[idx] > row$height[1L]
    }
  }
  d$outlier_flag <- flag
  x$gaze_samples <- d
  add_provenance(x, "flag_gaze_outliers", "gaze_samples", paste0("method=", method, ";n=", sum(flag, na.rm = TRUE)))
}

gaze_velocity <- function(data) {
  if (is_eye_dataset(data)) data <- data$gaze_samples
  .assert_data_frame(data, "data")
  .assert_columns(data, c("recording_id", "sample_id", "timestamp_seconds", "gaze_x", "gaze_y"))
  out <- vector("list", length(unique(data$recording_id))); k <- 0L
  for (rec in unique(data$recording_id)) {
    z <- data[data$recording_id == rec, ]; z <- z[order(z$timestamp_seconds), ]
    dt <- c(NA_real_, diff(z$timestamp_seconds))
    dist <- c(NA_real_, sqrt(diff(z$gaze_x)^2 + diff(z$gaze_y)^2))
    k <- k + 1L
    out[[k]] <- data.frame(
      recording_id = rec, sample_id = z$sample_id, timestamp_seconds = z$timestamp_seconds,
      dx = c(NA_real_, diff(z$gaze_x)), dy = c(NA_real_, diff(z$gaze_y)),
      distance = dist, dt = dt, velocity = dist / dt, stringsAsFactors = FALSE
    )
  }
  do.call(rbind, out)
}

interpolate_pupil <- function(x, method = c("linear", "constant", "none"), max_gap_ms = 150, mark = TRUE) {
  .assert_eye_dataset(x)
  method <- match.arg(method)
  if (method == "none" || !nrow(x$eye_samples)) return(x)
  d <- x$eye_samples
  if (!"interpolated" %in% names(d)) d$interpolated <- FALSE
  groups <- split(seq_len(nrow(d)), interaction(d$recording_id, d$eye, drop = TRUE))
  for (idx in groups) {
    ord <- idx[order(d$timestamp_seconds[idx])]
    t <- d$timestamp_seconds[ord]; y <- d$pupil_diameter[ord]
    missing <- !is.finite(y)
    if (!any(missing) || all(missing)) next
    runs <- rle(missing); ends <- cumsum(runs$lengths); starts <- c(1L, head(ends, -1L) + 1L)
    for (r in which(runs$values)) {
      pos <- starts[r]:ends[r]
      if (min(pos) == 1L || max(pos) == length(y)) next
      gap <- (t[max(pos) + 1L] - t[min(pos) - 1L]) * 1000
      if (!is.finite(gap) || gap > max_gap_ms) next
      if (method == "linear") {
        y[pos] <- stats::approx(t[c(min(pos) - 1L, max(pos) + 1L)], y[c(min(pos) - 1L, max(pos) + 1L)], xout = t[pos], rule = 1)$y
      } else {
        y[pos] <- y[min(pos) - 1L]
      }
      if (mark) d$interpolated[ord[pos]] <- TRUE
    }
    d$pupil_diameter[ord] <- y
  }
  x$eye_samples <- d
  add_provenance(x, "interpolate_pupil", "eye_samples", paste0("method=", method, ";max_gap_ms=", max_gap_ms), reversible = FALSE)
}

filter_pupil <- function(x, method = c("median", "mean", "moving_median", "moving_average", "none"), window = 5L) {
  .assert_eye_dataset(x)
  method <- match.arg(method)
  if (method == "moving_median") method <- "median"
  if (method == "moving_average") method <- "mean"
  if (method == "none" || !nrow(x$eye_samples)) return(x)
  d <- x$eye_samples
  groups <- split(seq_len(nrow(d)), interaction(d$recording_id, d$eye, drop = TRUE))
  fun <- if (method == "median") stats::median else mean
  if (!"pupil_raw" %in% names(d)) d$pupil_raw <- d$pupil_diameter
  for (idx in groups) {
    ord <- idx[order(d$timestamp_seconds[idx])]
    d$pupil_diameter[ord] <- rolling_apply(d$pupil_diameter[ord], window, fun)
  }
  x$eye_samples <- d
  add_provenance(x, "filter_pupil", "eye_samples", paste0("method=", method, ";window=", window), reversible = FALSE)
}

baseline_pupil <- function(
    x,
    method = c("subtract", "divide", "percent", "zscore", "none"),
    baseline_window = c(-0.2, 0),
    anchor = c("trial_start", "recording_start"),
    minimum_samples = 3L) {
  .assert_eye_dataset(x)
  method <- match.arg(method); anchor <- match.arg(anchor)
  if (method == "none" || !nrow(x$eye_samples)) return(x)
  d <- x$eye_samples
  if (!"pupil_uncorrected" %in% names(d)) d$pupil_uncorrected <- d$pupil_diameter
  if (anchor == "trial_start") {
    trials <- x$intervals[x$intervals$interval_type == "trial", c("recording_id", "trial_id", "start_time")]
    if (!nrow(trials)) .eye_stop("Trial intervals are required for trial-start baseline correction.")
    key <- paste(d$recording_id, d$trial_id, sep = "\r")
    tkey <- paste(trials$recording_id, trials$trial_id, sep = "\r")
    start <- trials$start_time[match(key, tkey)]
  } else {
    start <- ave(d$timestamp_seconds, d$recording_id, FUN = function(z) min(z, na.rm = TRUE))
  }
  relative <- d$timestamp_seconds - start
  groups <- split(seq_len(nrow(d)), interaction(d$recording_id, d$trial_id, d$eye, drop = TRUE))
  d$pupil_baseline <- NA_real_
  for (idx in groups) {
    bidx <- idx[relative[idx] >= baseline_window[1L] & relative[idx] <= baseline_window[2L] & is.finite(d$pupil_diameter[idx])]
    if (length(bidx) < minimum_samples) next
    base <- mean(d$pupil_diameter[bidx], na.rm = TRUE)
    d$pupil_baseline[idx] <- base
    if (method == "subtract") d$pupil_diameter[idx] <- d$pupil_diameter[idx] - base
    if (method == "divide") d$pupil_diameter[idx] <- d$pupil_diameter[idx] / base
    if (method == "percent") d$pupil_diameter[idx] <- (d$pupil_diameter[idx] - base) / base * 100
    if (method == "zscore") {
      s <- stats::sd(d$pupil_diameter[bidx], na.rm = TRUE)
      d$pupil_diameter[idx] <- (d$pupil_diameter[idx] - base) / s
    }
  }
  x$eye_samples <- d
  add_provenance(x, "baseline_pupil", "eye_samples", paste0("method=", method, ";window=", paste(baseline_window, collapse = ",")), reversible = method %in% c("subtract", "divide", "percent"))
}

pupil_deconvolve <- function(x, tau = 0.9, regularization = 0.01, output_column = "pupil_phasic") {
  .assert_eye_dataset(x)
  d <- x$eye_samples
  if (!nrow(d)) return(x)
  groups <- split(seq_len(nrow(d)), interaction(d$recording_id, d$eye, drop = TRUE))
  d[[output_column]] <- NA_real_
  for (idx in groups) {
    ord <- idx[order(d$timestamp_seconds[idx])]
    y <- d$pupil_diameter[ord]
    t <- d$timestamp_seconds[ord]
    if (sum(is.finite(y)) < 3L) next
    dt <- stats::median(diff(t), na.rm = TRUE)
    alpha <- exp(-dt / tau)
    yy <- y
    yy[!is.finite(yy)] <- stats::median(yy, na.rm = TRUE)
    innovation <- c(0, diff(yy) + (1 - alpha) * head(yy, -1L))
    innovation <- innovation / (1 + regularization)
    d[[output_column]][ord] <- innovation
  }
  x$eye_samples <- d
  add_provenance(x, "pupil_deconvolve", "eye_samples", paste0("tau=", tau, ";regularization=", regularization), reversible = FALSE,
    warnings = "Exploratory discrete deconvolution; validate assumptions before substantive interpretation.")
}

detect_blinks <- function(x, min_duration_ms = 50, max_duration_ms = 1000, source = c("pupil_missing", "validity"), overwrite = FALSE) {
  .assert_eye_dataset(x)
  source <- match.arg(source)
  d <- x$eye_samples
  if (!nrow(d)) return(x)
  episodes <- list(); k <- 0L
  groups <- split(d, interaction(d$recording_id, d$eye, drop = TRUE))
  for (z in groups) {
    z <- z[order(z$timestamp_seconds), ]
    missing <- if (source == "pupil_missing") !is.finite(z$pupil_diameter) else !z$pupil_valid | is.na(z$pupil_valid)
    runs <- rle(missing); ends <- cumsum(runs$lengths); starts <- c(1L, head(ends, -1L) + 1L)
    for (r in which(runs$values)) {
      pos <- starts[r]:ends[r]
      dur <- (z$timestamp_seconds[max(pos)] - z$timestamp_seconds[min(pos)]) * 1000
      if (!is.finite(dur) || dur < min_duration_ms || dur > max_duration_ms) next
      k <- k + 1L
      episodes[[k]] <- data.frame(
        episode_id = paste0(z$recording_id[1L], "_blink_", z$eye[1L], "_", sprintf("%06d", k)),
        recording_id = z$recording_id[1L], episode_type = "blink", eye = z$eye[1L],
        start_time = z$timestamp_seconds[min(pos)], end_time = z$timestamp_seconds[max(pos)], duration_ms = dur,
        start_x = NA_real_, start_y = NA_real_, end_x = NA_real_, end_y = NA_real_,
        centroid_x = NA_real_, centroid_y = NA_real_, amplitude = NA_real_, peak_velocity = NA_real_, dispersion = NA_real_,
        coordinate_space_id = NA_character_, source_algorithm = paste0("eyeprocess_", source),
        source_parameters = paste0("min=", min_duration_ms, ";max=", max_duration_ms), derived_by = "eyeprocess",
        trial_id = .mode_value(z$trial_id[pos]), stimulus_id = .mode_value(z$stimulus_id[pos]), aoi_id = NA_character_,
        stringsAsFactors = FALSE
      )
    }
  }
  if (overwrite) x$episodes <- x$episodes[x$episodes$episode_type != "blink" | x$episodes$derived_by != "eyeprocess", ]
  if (length(episodes)) x$episodes <- standardize_eye_table(.bind_rows_base(x$episodes, do.call(.bind_rows_base, episodes)), "episodes")
  add_provenance(x, "detect_blinks", "episodes", paste0(k, " blinks; source=", source))
}

detect_fixations_ivt <- function(
    x,
    velocity_threshold = 30,
    minimum_duration_ms = 60,
    maximum_gap_ms = 75,
    coordinate_units = c("degrees", "pixels", "normalized"),
    overwrite = FALSE) {
  .assert_eye_dataset(x)
  coordinate_units <- match.arg(coordinate_units)
  d <- x$gaze_samples
  if (!nrow(d)) return(x)
  if (coordinate_units != "degrees") .eye_warn("I-VT threshold is being applied in `", coordinate_units, "` per second, not visual degrees per second.")
  episodes <- list(); k <- 0L
  groups <- split(d, interaction(d$recording_id, d$trial_id, drop = TRUE))
  for (z in groups) {
    z <- z[order(z$timestamp_seconds), ]
    dt <- c(NA_real_, diff(z$timestamp_seconds))
    velocity <- c(NA_real_, sqrt(diff(z$gaze_x)^2 + diff(z$gaze_y)^2) / diff(z$timestamp_seconds))
    is_fix <- is.finite(velocity) & velocity <= velocity_threshold & z$valid
    is_fix[1L] <- if (length(is_fix) > 1L) is_fix[2L] else FALSE
    breaks <- c(TRUE, !is_fix[-1L] | !is_fix[-length(is_fix)] | dt[-1L] * 1000 > maximum_gap_ms)
    run <- cumsum(breaks)
    for (r in unique(run[is_fix])) {
      pos <- which(run == r & is_fix)
      if (!length(pos)) next
      dur <- (max(z$timestamp_seconds[pos]) - min(z$timestamp_seconds[pos])) * 1000
      if (dur < minimum_duration_ms) next
      k <- k + 1L
      episodes[[k]] <- data.frame(
        episode_id = paste0(z$recording_id[1L], "_ivt_fix_", sprintf("%07d", k)),
        recording_id = z$recording_id[1L], episode_type = "fixation", eye = "combined",
        start_time = min(z$timestamp_seconds[pos]), end_time = max(z$timestamp_seconds[pos]), duration_ms = dur,
        start_x = z$gaze_x[min(pos)], start_y = z$gaze_y[min(pos)], end_x = z$gaze_x[max(pos)], end_y = z$gaze_y[max(pos)],
        centroid_x = mean(z$gaze_x[pos], na.rm = TRUE), centroid_y = mean(z$gaze_y[pos], na.rm = TRUE),
        amplitude = NA_real_, peak_velocity = .safe_max(velocity[pos]),
        dispersion = .safe_span(z$gaze_x[pos]) + .safe_span(z$gaze_y[pos]),
        coordinate_space_id = z$coordinate_space_id[1L], source_algorithm = "I-VT",
        source_parameters = paste0("velocity_threshold=", velocity_threshold, ";units=", coordinate_units, ";minimum_duration_ms=", minimum_duration_ms),
        derived_by = "eyeprocess", trial_id = z$trial_id[1L], stimulus_id = .mode_value(z$stimulus_id[pos]), aoi_id = NA_character_,
        stringsAsFactors = FALSE
      )
    }
  }
  if (overwrite) x$episodes <- x$episodes[!(x$episodes$episode_type == "fixation" & x$episodes$derived_by == "eyeprocess"), ]
  if (length(episodes)) x$episodes <- standardize_eye_table(.bind_rows_base(x$episodes, do.call(.bind_rows_base, episodes)), "episodes")
  add_provenance(x, "detect_fixations_ivt", "episodes", paste0(k, " fixations"), warnings = if (coordinate_units != "degrees") "Threshold units are not visual degrees." else NA_character_)
}

detect_fixations_idt <- function(
    x,
    dispersion_threshold = 1,
    minimum_duration_ms = 100,
    coordinate_units = c("degrees", "pixels", "normalized"),
    overwrite = FALSE) {
  .assert_eye_dataset(x)
  coordinate_units <- match.arg(coordinate_units)
  d <- x$gaze_samples
  if (!nrow(d)) return(x)
  episodes <- list(); k <- 0L
  groups <- split(d, interaction(d$recording_id, d$trial_id, drop = TRUE))
  for (z in groups) {
    z <- z[order(z$timestamp_seconds), ]
    n <- nrow(z); i <- 1L
    while (i <= n) {
      j <- i
      while (j < n && (z$timestamp_seconds[j] - z$timestamp_seconds[i]) * 1000 < minimum_duration_ms) j <- j + 1L
      if (j > n) break
      disp <- .safe_span(z$gaze_x[i:j]) + .safe_span(z$gaze_y[i:j])
      if (!is.finite(disp) || disp > dispersion_threshold) { i <- i + 1L; next }
      while (j < n) {
        cand <- i:(j + 1L)
        d2 <- .safe_span(z$gaze_x[cand]) + .safe_span(z$gaze_y[cand])
        if (!is.finite(d2) || d2 > dispersion_threshold) break
        j <- j + 1L; disp <- d2
      }
      pos <- i:j; k <- k + 1L
      episodes[[k]] <- data.frame(
        episode_id = paste0(z$recording_id[1L], "_idt_fix_", sprintf("%07d", k)), recording_id = z$recording_id[1L],
        episode_type = "fixation", eye = "combined", start_time = z$timestamp_seconds[i], end_time = z$timestamp_seconds[j],
        duration_ms = (z$timestamp_seconds[j] - z$timestamp_seconds[i]) * 1000,
        start_x = z$gaze_x[i], start_y = z$gaze_y[i], end_x = z$gaze_x[j], end_y = z$gaze_y[j],
        centroid_x = mean(z$gaze_x[pos], na.rm = TRUE), centroid_y = mean(z$gaze_y[pos], na.rm = TRUE),
        amplitude = NA_real_, peak_velocity = NA_real_, dispersion = disp,
        coordinate_space_id = z$coordinate_space_id[1L], source_algorithm = "I-DT",
        source_parameters = paste0("dispersion_threshold=", dispersion_threshold, ";units=", coordinate_units, ";minimum_duration_ms=", minimum_duration_ms),
        derived_by = "eyeprocess", trial_id = z$trial_id[1L], stimulus_id = .mode_value(z$stimulus_id[pos]), aoi_id = NA_character_, stringsAsFactors = FALSE
      )
      i <- j + 1L
    }
  }
  if (overwrite) x$episodes <- x$episodes[!(x$episodes$episode_type == "fixation" & x$episodes$derived_by == "eyeprocess"), ]
  if (length(episodes)) x$episodes <- standardize_eye_table(.bind_rows_base(x$episodes, do.call(.bind_rows_base, episodes)), "episodes")
  add_provenance(x, "detect_fixations_idt", "episodes", paste0(k, " fixations"), warnings = if (coordinate_units != "degrees") "Dispersion threshold units are not visual degrees." else NA_character_)
}

detect_saccades <- function(x, velocity_threshold = 30, minimum_duration_ms = 10, overwrite = FALSE) {
  .assert_eye_dataset(x)
  d <- x$gaze_samples
  if (!nrow(d)) return(x)
  vel <- gaze_velocity(d)
  episodes <- list(); k <- 0L
  groups <- split(vel, vel$recording_id)
  for (z in groups) {
    z <- z[order(z$timestamp_seconds), ]
    high <- is.finite(z$velocity) & z$velocity > velocity_threshold
    run <- cumsum(c(TRUE, high[-1L] != high[-length(high)]))
    for (r in unique(run[high])) {
      pos <- which(run == r & high)
      dur <- (max(z$timestamp_seconds[pos]) - min(z$timestamp_seconds[pos])) * 1000
      if (dur < minimum_duration_ms) next
      orig <- d[match(z$sample_id[pos], d$sample_id), ]
      k <- k + 1L
      episodes[[k]] <- data.frame(
        episode_id = paste0(z$recording_id[1L], "_saccade_", sprintf("%07d", k)), recording_id = z$recording_id[1L],
        episode_type = "saccade", eye = "combined", start_time = min(z$timestamp_seconds[pos]), end_time = max(z$timestamp_seconds[pos]), duration_ms = dur,
        start_x = orig$gaze_x[1L], start_y = orig$gaze_y[1L], end_x = orig$gaze_x[nrow(orig)], end_y = orig$gaze_y[nrow(orig)],
        centroid_x = NA_real_, centroid_y = NA_real_, amplitude = sqrt((orig$gaze_x[nrow(orig)] - orig$gaze_x[1L])^2 + (orig$gaze_y[nrow(orig)] - orig$gaze_y[1L])^2),
        peak_velocity = .safe_max(z$velocity[pos]), dispersion = NA_real_, coordinate_space_id = orig$coordinate_space_id[1L],
        source_algorithm = "velocity threshold", source_parameters = paste0("threshold=", velocity_threshold), derived_by = "eyeprocess",
        trial_id = .mode_value(orig$trial_id), stimulus_id = .mode_value(orig$stimulus_id), aoi_id = NA_character_, stringsAsFactors = FALSE
      )
    }
  }
  if (overwrite) x$episodes <- x$episodes[!(x$episodes$episode_type == "saccade" & x$episodes$derived_by == "eyeprocess"), ]
  if (length(episodes)) x$episodes <- standardize_eye_table(.bind_rows_base(x$episodes, do.call(.bind_rows_base, episodes)), "episodes")
  add_provenance(x, "detect_saccades", "episodes", paste0(k, " saccades"))
}

preprocess_eye <- function(x, spec = preprocess_spec()) {
  .assert_eye_dataset(x)
  if (!inherits(spec, "eye_preprocess_spec")) .eye_stop("`spec` must be created with `preprocess_spec()`.")
  x <- filter_gaze(x, spec$gaze_filter, spec$gaze_window)
  x <- interpolate_pupil(x, spec$pupil_interpolation, spec$pupil_max_gap_ms)
  x <- filter_pupil(x, spec$pupil_filter, spec$pupil_window)
  if (spec$pupil_baseline != "none" && nrow(x$intervals)) x <- baseline_pupil(x, spec$pupil_baseline, spec$pupil_baseline_window)
  if (spec$blink_detection) x <- detect_blinks(x)
  if (spec$fixation_algorithm == "ivt") x <- do.call(detect_fixations_ivt, c(list(x = x), spec$fixation_parameters))
  if (spec$fixation_algorithm == "idt") x <- do.call(detect_fixations_idt, c(list(x = x), spec$fixation_parameters))
  add_provenance(x, "preprocess_eye", "dataset", paste(capture.output(str(spec)), collapse = " "), reversible = FALSE)
}
