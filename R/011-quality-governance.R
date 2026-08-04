.quality_row <- function(recording_id, trial_id = NA_character_, stream_id = NA_character_, metric, value, threshold, status, message) {
  data.frame(
    quality_id = .next_id("quality"),
    recording_id = recording_id, trial_id = trial_id, stream_id = stream_id,
    metric = metric, value = as.numeric(value), threshold = as.numeric(threshold),
    status = status, message = message, computed_at = .now_utc(), stringsAsFactors = FALSE
  )
}

store_quality <- function(x, report, replace_metric = FALSE) {
  .assert_eye_dataset(x); .assert_data_frame(report, "report")
  if (!all(schema_table("quality") %in% names(report))) {
    report <- standardize_eye_table(report, "quality")
  }
  if (replace_metric && nrow(report)) x$quality <- x$quality[!x$quality$metric %in% unique(report$metric), ]
  x$quality <- standardize_eye_table(.bind_rows_base(x$quality, report), "quality")
  add_provenance(x, "store_quality", "quality", paste0(nrow(report), " rows"))
}

audit_sampling_rate <- function(x, expected_hz = NULL, tolerance_hz = 5, store = FALSE) {
  .assert_eye_dataset(x)
  d <- x$gaze_samples
  if (!nrow(d)) return(data.frame())
  groups <- split(d, d$recording_id)
  report <- do.call(rbind, lapply(groups, function(z) {
    observed <- estimate_sampling_rate(z$timestamp_seconds)
    expected <- expected_hz
    if (is.null(expected)) expected <- .first_nonmissing(x$recordings$nominal_sampling_rate[x$recordings$recording_id == z$recording_id[1L]], NA_real_)
    diff <- abs(observed - expected)
    status <- if (!is.finite(expected)) "unknown" else if (diff <= tolerance_hz) "ok" else "warning"
    .quality_row(
      z$recording_id[1L], stream_id = .first_nonmissing(z$stream_id), metric = "sampling_rate_hz",
      value = observed, threshold = if (is.finite(expected)) expected else NA_real_, status = status,
      message = if (status == "ok") "Observed sampling rate is within tolerance." else if (status == "warning") paste0("Observed rate differs from expected by ", round(diff, 2), " Hz.") else "Expected sampling rate is unavailable."
    )
  }))
  if (store) return(store_quality(x, report, replace_metric = TRUE))
  report
}

audit_signal_quality <- function(
    x,
    minimum_valid_gaze = 0.80,
    minimum_valid_pupil = 0.70,
    by_trial = TRUE,
    store = FALSE) {
  .assert_eye_dataset(x)
  rows <- list(); k <- 0L
  if (nrow(x$gaze_samples)) {
    keys <- if (by_trial && any(!is.na(x$gaze_samples$trial_id))) c("recording_id", "trial_id") else "recording_id"
    groups <- .group_split(x$gaze_samples, keys)
    for (z in groups) {
      valid <- z$valid & is.finite(z$gaze_x) & is.finite(z$gaze_y)
      p <- mean(valid, na.rm = TRUE)
      k <- k + 1L
      rows[[k]] <- .quality_row(z$recording_id[1L], if ("trial_id" %in% keys) z$trial_id[1L] else NA_character_,
        .first_nonmissing(z$stream_id), "valid_gaze_fraction", p, minimum_valid_gaze,
        if (is.finite(p) && p >= minimum_valid_gaze) "ok" else "warning",
        paste0(round(100 * p, 1), "% valid gaze samples."))
    }
  }
  if (nrow(x$eye_samples)) {
    keys <- if (by_trial && any(!is.na(x$eye_samples$trial_id))) c("recording_id", "trial_id", "eye") else c("recording_id", "eye")
    groups <- .group_split(x$eye_samples, keys)
    for (z in groups) {
      valid <- z$pupil_valid & is.finite(z$pupil_diameter)
      p <- mean(valid, na.rm = TRUE)
      k <- k + 1L
      rows[[k]] <- .quality_row(z$recording_id[1L], if ("trial_id" %in% keys) z$trial_id[1L] else NA_character_,
        paste0(z$recording_id[1L], "_pupil_", z$eye[1L]), paste0("valid_pupil_fraction_", z$eye[1L]), p, minimum_valid_pupil,
        if (is.finite(p) && p >= minimum_valid_pupil) "ok" else "warning",
        paste0(round(100 * p, 1), "% valid pupil observations (", z$eye[1L], ")."))
    }
  }
  report <- if (length(rows)) do.call(.bind_rows_base, rows) else empty_eye_table("quality")
  if (store) return(store_quality(x, report, replace_metric = TRUE))
  report
}

audit_pupil_quality <- function(x, maximum_interpolated_fraction = 0.20, plausible_range = NULL, store = FALSE) {
  .assert_eye_dataset(x)
  d <- x$eye_samples
  if (!nrow(d)) return(data.frame())
  groups <- .group_split(d, c("recording_id", "eye"))
  rows <- list(); k <- 0L
  for (z in groups) {
    interpolated <- if ("interpolated" %in% names(z)) mean(z$interpolated, na.rm = TRUE) else 0
    k <- k + 1L
    rows[[k]] <- .quality_row(z$recording_id[1L], stream_id = paste0(z$recording_id[1L], "_pupil_", z$eye[1L]),
      metric = paste0("interpolated_pupil_fraction_", z$eye[1L]), value = interpolated,
      threshold = maximum_interpolated_fraction,
      status = if (interpolated <= maximum_interpolated_fraction) "ok" else "warning",
      message = paste0(round(100 * interpolated, 1), "% interpolated pupil observations."))
    if (!is.null(plausible_range)) {
      out <- mean(z$pupil_diameter < plausible_range[1L] | z$pupil_diameter > plausible_range[2L], na.rm = TRUE)
      k <- k + 1L
      rows[[k]] <- .quality_row(z$recording_id[1L], stream_id = paste0(z$recording_id[1L], "_pupil_", z$eye[1L]),
        metric = paste0("out_of_range_pupil_fraction_", z$eye[1L]), value = out, threshold = 0,
        status = if (out == 0) "ok" else "warning",
        message = paste0(round(100 * out, 1), "% outside declared plausible range."))
    }
  }
  report <- do.call(.bind_rows_base, rows)
  if (store) return(store_quality(x, report, replace_metric = TRUE))
  report
}

audit_episodes <- function(x, type = NULL) {
  .assert_eye_dataset(x)
  d <- x$episodes
  if (!is.null(type)) d <- d[d$episode_type %in% type, ]
  if (!nrow(d)) return(data.frame())
  data.frame(
    episode_type = unique(d$episode_type),
    n = vapply(unique(d$episode_type), function(t) sum(d$episode_type == t), integer(1)),
    n_negative_duration = vapply(unique(d$episode_type), function(t) sum(d$episode_type == t & d$duration_ms < 0, na.rm = TRUE), integer(1)),
    n_missing_coordinates = vapply(unique(d$episode_type), function(t) sum(d$episode_type == t & (!is.finite(d$centroid_x) | !is.finite(d$centroid_y))), integer(1)),
    vendor_derived = vapply(unique(d$episode_type), function(t) sum(d$episode_type == t & d$derived_by == "vendor", na.rm = TRUE), integer(1)),
    package_derived = vapply(unique(d$episode_type), function(t) sum(d$episode_type == t & d$derived_by == "eyeprocess", na.rm = TRUE), integer(1)),
    stringsAsFactors = FALSE
  )
}

audit_event_order <- function(x, event_type = NULL) {
  .assert_eye_dataset(x)
  d <- x$events
  if (!is.null(event_type)) d <- d[d$event_type %in% event_type, ]
  if (!nrow(d)) return(data.frame())
  groups <- split(d, d$recording_id)
  do.call(rbind, lapply(groups, function(z) data.frame(
    recording_id = z$recording_id[1L], n_events = nrow(z),
    n_nonmonotonic = sum(diff(z$timestamp_seconds[is.finite(z$timestamp_seconds)]) < 0),
    n_duplicate_timestamps = sum(duplicated(z$timestamp_seconds[is.finite(z$timestamp_seconds)])),
    status = if (sum(diff(z$timestamp_seconds[is.finite(z$timestamp_seconds)]) < 0) > 0) "warning" else "ok",
    stringsAsFactors = FALSE
  )))
}

audit_trial_coverage <- function(x) {
  .assert_eye_dataset(x)
  trials <- trial_table(x)
  if (!nrow(trials)) return(data.frame(status = "error", message = "No trials defined."))
  data.frame(
    recording_id = trials$recording_id,
    trial_id = trials$trial_id,
    duration_seconds = trials$end_time - trials$start_time,
    n_gaze_samples = mapply(function(r, t) sum(x$gaze_samples$recording_id == r & x$gaze_samples$trial_id == t, na.rm = TRUE), trials$recording_id, trials$trial_id),
    n_eye_samples = mapply(function(r, t) sum(x$eye_samples$recording_id == r & x$eye_samples$trial_id == t, na.rm = TRUE), trials$recording_id, trials$trial_id),
    n_episodes = mapply(function(r, t) sum(x$episodes$recording_id == r & x$episodes$trial_id == t, na.rm = TRUE), trials$recording_id, trials$trial_id),
    has_response = mapply(function(r, t) any(x$responses$recording_id == r & x$responses$trial_id == t), trials$recording_id, trials$trial_id),
    status = ifelse(trials$valid_interval & is.finite(trials$end_time - trials$start_time), "ok", "error"),
    stringsAsFactors = FALSE
  )
}

audit_aois <- function(x) {
  .assert_eye_dataset(x)
  defs <- x$aoi_definitions; geom <- x$aoi_geometry
  if (!nrow(defs)) return(data.frame(status = "unavailable", message = "No AOIs registered."))
  data.frame(
    aoi_id = defs$aoi_id, aoi_name = defs$aoi_name, shape_type = defs$shape_type,
    has_geometry = defs$aoi_id %in% geom$aoi_id,
    n_geometry_records = vapply(defs$aoi_id, function(id) sum(geom$aoi_id == id), integer(1)),
    coordinate_registered = defs$coordinate_space_id %in% x$coordinate_spaces$coordinate_space_id,
    status = ifelse(defs$aoi_id %in% geom$aoi_id & defs$coordinate_space_id %in% x$coordinate_spaces$coordinate_space_id, "ok", "error"),
    stringsAsFactors = FALSE
  )
}

audit_missingness <- function(x, component = c("gaze_samples", "eye_samples", "biometrics"), by = "recording_id") {
  .assert_eye_dataset(x)
  component <- match.arg(component)
  d <- x[[component]]
  if (!nrow(d)) return(data.frame())
  by <- intersect(by, names(d)); groups <- if (length(by)) .group_split(d, by) else list(all = d)
  out <- lapply(groups, function(z) {
    cols <- names(z)
    miss <- vapply(z, function(v) mean(is.na(v) | (is.numeric(v) & !is.finite(v))), numeric(1))
    data.frame(
      component = component,
      group = if (length(by)) paste(unlist(z[1L, by]), collapse = "|") else "all",
      field = cols, missing_fraction = miss, stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

check_process_leakage <- function(x, response_time_tolerance = 0) {
  .assert_eye_dataset(x)
  f <- x$features; r <- x$responses
  if (!nrow(f) || !nrow(r)) return(data.frame())
  key_f <- paste(f$recording_id, f$trial_id, sep = "\r")
  key_r <- paste(r$recording_id, r$trial_id, sep = "\r")
  response_ts <- r$response_timestamp[match(key_f, key_r)]
  leaked <- is.finite(f$window_end) & is.finite(response_ts) & f$window_end > response_ts + response_time_tolerance
  data.frame(
    feature_id = f$feature_id, feature_name = f$feature_name,
    response_timestamp = response_ts, feature_window_end = f$window_end,
    post_response = leaked, status = ifelse(leaked, "error", "ok"),
    message = ifelse(leaked, "Feature window extends beyond response time.", "No post-response leakage detected from recorded windows."),
    stringsAsFactors = FALSE
  )
}

check_feature_level <- function(x) {
  .assert_eye_dataset(x)
  f <- x$features
  if (!nrow(f)) return(data.frame())
  expected <- list(
    trial = c("recording_id", "trial_id"), trial_aoi = c("recording_id", "trial_id", "aoi_id"),
    trial_eye = c("recording_id", "trial_id"), response = c("participant_id", "item_id")
  )
  rows <- lapply(seq_len(nrow(f)), function(i) {
    fields <- expected[[f$level[i]]] %||% character()
    missing <- fields[vapply(fields, function(nm) is.na(f[[nm]][i]) || !nzchar(as.character(f[[nm]][i])), logical(1))]
    data.frame(
      feature_id = f$feature_id[i], feature_name = f$feature_name[i], level = f$level[i],
      missing_keys = paste(missing, collapse = ","),
      status = if (length(missing)) "warning" else "ok", stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

interpretive_warnings <- function() {
  data.frame(
    warning_id = paste0("interpretation_", sprintf("%02d", seq_len(7L))),
    observation = c("fixation", "dwell time", "pupil dilation", "rapid response", "EDA/heart rate", "latent process factor", "gaze-derived class"),
    prohibited_automatic_interpretation = c("attention", "difficulty", "cognitive load", "guessing", "specific emotion or diagnosis", "effort/engagement/arousal", "cognitive strategy"),
    guidance = c(
      "Interpret fixation within task, stimulus, and measurement context.",
      "Longer dwell may reflect difficulty, interest, confusion, rereading, or design properties.",
      "Control luminance, baseline, blink handling, timing, and alternative arousal explanations.",
      "Use task-specific evidence and model speed-accuracy relations explicitly.",
      "Physiological signals are nonspecific and require validated context-sensitive interpretation.",
      "Name factors neutrally until construct validity is demonstrated.",
      "Validate classes externally and assess stability and preprocessing dependence."
    ), stringsAsFactors = FALSE
  )
}

analysis_readiness <- function(x) {
  .assert_eye_dataset(x)
  validation <- validate_eye_dataset(x)
  gaze_quality <- audit_signal_quality(x)
  coord <- audit_coordinate_spaces(x)
  time <- audit_timebase(x)
  trials <- audit_trial_coverage(x)
  checks <- data.frame(
    domain = c("schema", "recordings", "timestamps", "coordinates", "trials", "responses", "gaze_quality", "provenance"),
    ready = c(
      !any(validation$severity == "error"),
      nrow(x$recordings) > 0,
      nrow(time) > 0 && !any(time$status == "warning"),
      nrow(coord) == 0 || all(coord$registered),
      nrow(trials) > 0 && !all(trials$status == "error"),
      nrow(x$responses) > 0,
      nrow(gaze_quality) == 0 || !all(gaze_quality$status == "warning"),
      nrow(x$provenance) > 0
    ),
    message = c(
      paste0(nrow(validation), " validation issue(s)."),
      paste0(nrow(x$recordings), " recording(s)."),
      paste0(nrow(time), " timebase audit row(s)."),
      paste0(nrow(coord), " coordinate-space use row(s)."),
      paste0(nrow(trials), " trial row(s)."),
      paste0(nrow(x$responses), " response row(s)."),
      paste0(nrow(gaze_quality), " signal-quality row(s)."),
      paste0(nrow(x$provenance), " provenance action(s).")
    ), stringsAsFactors = FALSE
  )
  class(checks) <- c("eye_readiness", "data.frame")
  checks
}

print.eye_readiness <- function(x, ...) {
  cat("eyeprocess analysis readiness\n")
  print.data.frame(x, row.names = FALSE)
  cat("Overall: ", if (all(x$ready)) "READY" else "NOT YET READY", "\n", sep = "")
  invisible(x)
}

compare_preprocessing <- function(..., metrics = c("valid_gaze_fraction", "valid_pupil_fraction", "fixation_count")) {
  xs <- list(...)
  if (length(xs) == 1L && is.list(xs[[1L]]) && !is_eye_dataset(xs[[1L]])) xs <- xs[[1L]]
  if (!all(vapply(xs, is_eye_dataset, logical(1)))) .eye_stop("All inputs must be `eye_dataset` objects.")
  rows <- lapply(seq_along(xs), function(i) {
    x <- xs[[i]]
    q <- audit_signal_quality(x)
    data.frame(
      pipeline = names(xs)[i] %||% paste0("pipeline_", i),
      valid_gaze_fraction = if (nrow(q)) mean(q$value[q$metric == "valid_gaze_fraction"], na.rm = TRUE) else NA_real_,
      valid_pupil_fraction = if (nrow(q)) mean(q$value[grepl("valid_pupil_fraction", q$metric)], na.rm = TRUE) else NA_real_,
      fixation_count = sum(x$episodes$episode_type == "fixation"),
      feature_rows = nrow(x$features), stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

compare_aoi_definitions <- function(..., source = "samples") {
  xs <- list(...)
  if (length(xs) == 1L && is.list(xs[[1L]]) && !is_eye_dataset(xs[[1L]])) xs <- xs[[1L]]
  rows <- lapply(seq_along(xs), function(i) {
    x <- xs[[i]]; .assert_eye_dataset(x)
    d <- if (source == "samples") x$gaze_samples else x$episodes
    if (!"aoi_id" %in% names(d)) return(data.frame())
    tab <- as.data.frame(table(d$aoi_id, useNA = "ifany"), stringsAsFactors = FALSE)
    names(tab) <- c("aoi_id", "count")
    tab$definition_set <- names(xs)[i] %||% paste0("set_", i)
    tab
  })
  do.call(.bind_rows_base, rows)
}

sensitivity_process <- function(..., label = NULL) {
  result <- compare_preprocessing(...)
  if (!is.null(label) && length(label) == nrow(result)) result$pipeline <- label
  structure(list(summary = result, compared_at = .now_utc()), class = "eye_sensitivity")
}

print.eye_sensitivity <- function(x, ...) {
  cat("eyeprocess sensitivity comparison\n")
  print(x$summary, row.names = FALSE)
  invisible(x)
}
