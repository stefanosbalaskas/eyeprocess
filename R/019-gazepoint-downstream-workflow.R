# Integrated Gazepoint downstream workflow --------------------------------

#' Specify an integrated Gazepoint downstream workflow
#'
#' Creates a declarative specification for the end-to-end Gazepoint workflow.
#' The defaults preserve vendor fixations, interpolate only short pupil gaps,
#' apply a small median pupil filter, and avoid automatic pupil baseline
#' correction when no pre-stimulus baseline is available.
#'
#' @param expected_sampling_rate Expected gaze sampling rate in hertz.
#' @param sampling_tolerance_hz Allowed absolute sampling-rate deviation.
#' @param minimum_valid_gaze Minimum acceptable valid-gaze fraction.
#' @param minimum_valid_pupil Minimum acceptable valid-pupil fraction.
#' @param pupil_interpolation Pupil interpolation method.
#' @param pupil_max_gap_ms Maximum pupil gap eligible for interpolation.
#' @param pupil_filter Pupil smoothing method.
#' @param pupil_window Smoothing window in samples.
#' @param pupil_baseline Baseline correction method. The default is `"none"`.
#' @param pupil_baseline_window Baseline window relative to media/trial onset.
#' @param detect_blinks Whether to derive blink episodes from missing pupil data.
#' @param biometric_channels Channels to retain in workflow plots and tables.
#' @param create_plots Whether to create the complete plot suite.
#' @param create_html_report Whether to render an HTML copy of the report when
#'   `rmarkdown` and Pandoc are available.
#' @param retain_raw Whether imported native exports are retained in the object.
#' @return An `eye_gazepoint_workflow_spec` object.
#' @export
gazepoint_workflow_spec <- function(
    expected_sampling_rate = 60,
    sampling_tolerance_hz = 5,
    minimum_valid_gaze = 0.80,
    minimum_valid_pupil = 0.70,
    pupil_interpolation = "linear",
    pupil_max_gap_ms = 150,
    pupil_filter = "median",
    pupil_window = 5L,
    pupil_baseline = "none",
    pupil_baseline_window = c(0, 0.5),
    detect_blinks = TRUE,
    biometric_channels = c(
      "eda", "skin_conductance_level", "skin_conductance_response",
      "heart_rate", "interbeat_interval", "engagement_dial"
    ),
    create_plots = TRUE,
    create_html_report = TRUE,
    retain_raw = TRUE) {
  pupil_interpolation <- match.arg(
    pupil_interpolation,
    c("linear", "constant", "none")
  )
  pupil_filter <- match.arg(
    pupil_filter,
    c("median", "mean", "moving_median", "moving_average", "none")
  )
  pupil_baseline <- match.arg(
    pupil_baseline,
    c("none", "subtract", "divide", "percent", "zscore")
  )
  if (length(pupil_baseline_window) != 2L ||
      any(!is.finite(pupil_baseline_window)) ||
      pupil_baseline_window[2L] < pupil_baseline_window[1L]) {
    .eye_stop("`pupil_baseline_window` must contain two ordered finite values.")
  }
  structure(
    list(
      expected_sampling_rate = as.numeric(expected_sampling_rate),
      sampling_tolerance_hz = as.numeric(sampling_tolerance_hz),
      minimum_valid_gaze = as.numeric(minimum_valid_gaze),
      minimum_valid_pupil = as.numeric(minimum_valid_pupil),
      pupil_interpolation = pupil_interpolation,
      pupil_max_gap_ms = as.numeric(pupil_max_gap_ms),
      pupil_filter = pupil_filter,
      pupil_window = as.integer(pupil_window),
      pupil_baseline = pupil_baseline,
      pupil_baseline_window = as.numeric(pupil_baseline_window),
      detect_blinks = isTRUE(detect_blinks),
      biometric_channels = unique(as.character(biometric_channels)),
      create_plots = isTRUE(create_plots),
      create_html_report = isTRUE(create_html_report),
      retain_raw = isTRUE(retain_raw)
    ),
    class = "eye_gazepoint_workflow_spec"
  )
}

#' @export
print.eye_gazepoint_workflow_spec <- function(x, ...) {
  cat("<eye_gazepoint_workflow_spec>\n")
  cat("  Expected rate:      ", x$expected_sampling_rate, " Hz\n", sep = "")
  cat("  Minimum valid gaze: ", x$minimum_valid_gaze, "\n", sep = "")
  cat("  Minimum valid pupil:", x$minimum_valid_pupil, "\n", sep = "")
  cat("  Pupil interpolation:", x$pupil_interpolation, "\n", sep = "")
  cat("  Pupil filter:       ", x$pupil_filter, "\n", sep = "")
  cat("  Pupil baseline:     ", x$pupil_baseline, "\n", sep = "")
  cat("  Create plots:       ", x$create_plots, "\n", sep = "")
  invisible(x)
}

.workflow_token <- function(x) {
  x <- as.character(x)
  x[is.na(x) | !nzchar(x)] <- "missing"
  gsub("_+", "_", gsub("[^A-Za-z0-9._-]+", "_", x))
}

.workflow_write_csv <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(x, path, row.names = FALSE, na = "")
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

.workflow_clean_output <- function(path, overwrite) {
  if (dir.exists(path) && length(list.files(path, all.files = TRUE, no.. = TRUE))) {
    if (!isTRUE(overwrite)) {
      .eye_stop("Output directory is not empty; use `overwrite = TRUE`: ", path)
    }
    unlink(path, recursive = TRUE, force = TRUE)
  }
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

.workflow_item_map <- function(item_map, stimuli) {
  stimuli <- unique(as.character(stimuli))
  stimuli <- stimuli[!is.na(stimuli) & nzchar(stimuli)]
  if (is.null(item_map)) {
    return(data.frame(
      stimulus_id = stimuli,
      item_id = stimuli,
      condition_id = NA_character_,
      stringsAsFactors = FALSE
    ))
  }
  if (is.character(item_map) && length(item_map) == 1L) {
    if (!file.exists(item_map)) .eye_stop("Item map does not exist: ", item_map)
    item_map <- utils::read.csv(item_map, stringsAsFactors = FALSE, check.names = FALSE)
  }
  .assert_data_frame(item_map, "item_map")
  .assert_columns(item_map, c("stimulus_id", "item_id"), "item_map")
  if (!"condition_id" %in% names(item_map)) item_map$condition_id <- NA_character_
  item_map$stimulus_id <- as.character(item_map$stimulus_id)
  item_map$item_id <- as.character(item_map$item_id)
  item_map$condition_id <- as.character(item_map$condition_id)
  if (anyDuplicated(item_map$stimulus_id)) {
    .eye_stop("`item_map$stimulus_id` must be unique.")
  }
  missing <- setdiff(stimuli, item_map$stimulus_id)
  if (length(missing)) {
    item_map <- .bind_rows_base(
      item_map,
      data.frame(
        stimulus_id = missing,
        item_id = missing,
        condition_id = NA_character_,
        stringsAsFactors = FALSE
      )
    )
  }
  item_map[c("stimulus_id", "item_id", "condition_id")]
}

#' Reconstruct media presentations as analysis trials
#'
#' Converts contiguous Gazepoint media runs into explicit trial intervals. This
#' provides stable person-by-item-by-trial keys even when the original export
#' contains no behavioural response file.
#'
#' @param x An `eye_dataset` imported from Gazepoint.
#' @param item_map Optional data frame or CSV path containing `stimulus_id`,
#'   `item_id`, and optionally `condition_id`.
#' @param overwrite Replace existing trial intervals.
#' @return The updated `eye_dataset`.
#' @export
build_gazepoint_media_trials <- function(x, item_map = NULL, overwrite = TRUE) {
  .assert_eye_dataset(x)
  d <- x$gaze_samples
  if (!nrow(d) || all(is.na(d$stimulus_id))) {
    .eye_stop("Gazepoint gaze samples do not contain media/stimulus identifiers.")
  }
  map <- .workflow_item_map(item_map, d$stimulus_id)
  intervals <- list()
  k <- 0L
  for (rec in unique(d$recording_id)) {
    z <- d[
      d$recording_id == rec &
        is.finite(d$timestamp_seconds) &
        !is.na(d$stimulus_id) &
        nzchar(as.character(d$stimulus_id)),
      , drop = FALSE
    ]
    z <- z[order(z$timestamp_seconds, z$sample_id), , drop = FALSE]
    if (!nrow(z)) next
    stimulus <- as.character(z$stimulus_id)
    time_break <- c(FALSE, diff(z$timestamp_seconds) < 0)
    run <- cumsum(c(TRUE, stimulus[-1L] != stimulus[-nrow(z)] | time_break[-1L]))
    run_index <- ave(run, stimulus, FUN = function(v) match(v, unique(v)))
    for (r in unique(run)) {
      q <- z[run == r, , drop = FALSE]
      stim <- as.character(q$stimulus_id[1L])
      map_row <- map[match(stim, map$stimulus_id), , drop = FALSE]
      participant <- .first_nonmissing(
        x$recordings$participant_id[x$recordings$recording_id == rec],
        NA_character_
      )
      this_run <- run_index[which(run == r)[1L]]
      trial_id <- paste0(
        rec, "__media_", .workflow_token(stim), "__run_",
        sprintf("%02d", this_run)
      )
      k <- k + 1L
      intervals[[k]] <- data.frame(
        interval_id = paste0("interval_", trial_id),
        recording_id = rec,
        interval_type = "trial",
        start_time = min(q$timestamp_seconds, na.rm = TRUE),
        end_time = max(q$timestamp_seconds, na.rm = TRUE),
        trial_id = trial_id,
        participant_id = participant,
        item_id = as.character(map_row$item_id[1L]),
        stimulus_id = stim,
        condition_id = as.character(map_row$condition_id[1L]),
        parent_interval_id = NA_character_,
        valid_interval = TRUE,
        source_method = "Gazepoint contiguous media run",
        stringsAsFactors = FALSE
      )
    }
  }
  built <- if (length(intervals)) {
    do.call(.bind_rows_base, intervals)
  } else {
    empty_eye_table("intervals")
  }
  if (!nrow(built)) .eye_stop("No Gazepoint media trials could be reconstructed.")
  if (isTRUE(overwrite)) {
    x$intervals <- x$intervals[x$intervals$interval_type != "trial", , drop = FALSE]
  }
  x$intervals <- standardize_eye_table(
    .bind_rows_base(x$intervals, built),
    "intervals"
  )
  x <- assign_trials(x, overwrite = TRUE)
  x <- add_provenance(
    x,
    "build_gazepoint_media_trials",
    "intervals",
    paste0(nrow(built), " media trials; item map rows=", nrow(map))
  )
  attr(x, "gazepoint_item_map") <- map
  x
}

.workflow_link_features_to_trials <- function(x) {
  if (!nrow(x$features)) return(x)
  trials <- trial_table(x)
  if (!nrow(trials)) return(x)
  f <- x$features
  missing_trial <- is.na(f$trial_id) | !nzchar(as.character(f$trial_id))
  for (i in which(missing_trial)) {
    candidate <- trials[FALSE, , drop = FALSE]
    if (!is.na(f$recording_id[i]) && nzchar(as.character(f$recording_id[i]))) {
      candidate <- trials[
        trials$recording_id == f$recording_id[i] &
          trials$stimulus_id == f$stimulus_id[i],
        , drop = FALSE
      ]
    }
    if (!nrow(candidate) && !is.na(f$participant_id[i])) {
      candidate <- trials[
        trials$participant_id == f$participant_id[i] &
          trials$stimulus_id == f$stimulus_id[i],
        , drop = FALSE
      ]
    }
    if (nrow(candidate) == 1L) {
      f$trial_id[i] <- candidate$trial_id[1L]
      f$item_id[i] <- candidate$item_id[1L]
      if (is.na(f$recording_id[i]) || !nzchar(f$recording_id[i])) {
        f$recording_id[i] <- candidate$recording_id[1L]
      }
    }
  }
  x$features <- standardize_eye_table(f, "features")
  x
}

.workflow_prepare_responses <- function(x, responses = NULL, score_key = NULL) {
  trials <- trial_table(x)
  template <- data.frame(
    recording_id = trials$recording_id,
    participant_id = trials$participant_id,
    trial_id = trials$trial_id,
    item_id = trials$item_id,
    stimulus_id = trials$stimulus_id,
    condition_id = trials$condition_id,
    response = NA_character_,
    score = NA_real_,
    response_time = NA_real_,
    trial_duration_seconds = trials$end_time - trials$start_time,
    stringsAsFactors = FALSE
  )
  if (is.null(responses)) {
    return(list(dataset = x, response_template = template, supplied = FALSE))
  }
  if (is.character(responses) && length(responses) == 1L) {
    if (!file.exists(responses)) .eye_stop("Response file does not exist: ", responses)
    responses <- utils::read.csv(responses, stringsAsFactors = FALSE, check.names = FALSE)
  }
  .assert_data_frame(responses, "responses")
  .assert_columns(responses, c("participant_id", "item_id"), "responses")
  if (!"response" %in% names(responses)) responses$response <- NA_character_
  if (!"score" %in% names(responses)) responses$score <- NA_real_
  if (!"response_time" %in% names(responses)) responses$response_time <- NA_real_
  if (!"trial_id" %in% names(responses)) responses$trial_id <- NA_character_
  if (!"recording_id" %in% names(responses)) responses$recording_id <- NA_character_
  responses$participant_id <- as.character(responses$participant_id)
  responses$item_id <- as.character(responses$item_id)
  responses$trial_id <- as.character(responses$trial_id)
  responses$recording_id <- as.character(responses$recording_id)
  for (i in seq_len(nrow(responses))) {
    missing_trial <- is.na(responses$trial_id[i]) || !nzchar(responses$trial_id[i])
    candidate <- trials[
      trials$participant_id == responses$participant_id[i] &
        trials$item_id == responses$item_id[i],
      , drop = FALSE
    ]
    if (!missing_trial) {
      candidate <- candidate[candidate$trial_id == responses$trial_id[i], , drop = FALSE]
    }
    if (nrow(candidate) != 1L) {
      .eye_stop(
        "Each supplied response must identify exactly one trial. Problem row: ", i,
        ". Supply `trial_id` when an item is repeated."
      )
    }
    responses$trial_id[i] <- candidate$trial_id[1L]
    responses$recording_id[i] <- candidate$recording_id[1L]
  }
  if (!is.null(score_key)) {
    if (is.null(names(score_key))) .eye_stop("`score_key` must be named by item id.")
    missing_score <- !is.finite(.safe_numeric(responses$score))
    correct <- as.character(score_key[responses$item_id])
    responses$score[missing_score] <- as.numeric(
      as.character(responses$response[missing_score]) == correct[missing_score]
    )
  }
  canonical <- data.frame(
    response_id = paste0("response_", .workflow_token(responses$trial_id)),
    recording_id = responses$recording_id,
    participant_id = responses$participant_id,
    trial_id = responses$trial_id,
    item_id = responses$item_id,
    response = as.character(responses$response),
    score = .safe_numeric(responses$score),
    response_time = .safe_numeric(responses$response_time),
    response_timestamp = NA_real_,
    response_type = "supplied",
    valid_response = TRUE,
    stringsAsFactors = FALSE
  )
  x <- add_responses(x, canonical, overwrite = TRUE)
  merged <- merge(
    template,
    responses[c("participant_id", "item_id", "trial_id", "response", "score", "response_time")],
    by = c("participant_id", "item_id", "trial_id"),
    all.x = TRUE,
    suffixes = c("_template", ""),
    sort = FALSE
  )
  merged$response_template <- NULL
  merged$score_template <- NULL
  merged$response_time_template <- NULL
  list(dataset = x, response_template = merged, supplied = TRUE)
}

.workflow_prepare_biometrics <- function(x) {
  if (!nrow(x$biometrics)) return(x)
  d <- x$biometrics
  valid <- d$valid %in% TRUE & is.finite(d$value)
  d$analysis_value <- ifelse(valid, d$value, NA_real_)
  d$analysis_valid <- valid
  x$biometrics <- d
  add_provenance(
    x,
    "prepare_workflow_biometrics",
    "biometrics",
    paste0(sum(valid), " valid observations retained in analysis_value; raw values preserved")
  )
}

.workflow_preprocess_pupil <- function(x, spec) {
  if (!nrow(x$eye_samples)) return(x)
  if (isTRUE(spec$detect_blinks)) {
    x <- detect_blinks(x, source = "validity", overwrite = TRUE)
  }
  d <- x$eye_samples
  if (!"pupil_native" %in% names(d)) d$pupil_native <- d$pupil_diameter
  invalid <- !(d$pupil_valid %in% TRUE) | !is.finite(d$pupil_diameter)
  d$pupil_diameter[invalid] <- NA_real_
  x$eye_samples <- d
  x <- add_provenance(
    x,
    "prepare_workflow_pupil",
    "eye_samples",
    paste0(sum(invalid), " invalid pupil values excluded from the analysis series; pupil_native preserved")
  )
  x <- interpolate_pupil(
    x,
    method = spec$pupil_interpolation,
    max_gap_ms = spec$pupil_max_gap_ms,
    mark = TRUE
  )
  x <- filter_pupil(
    x,
    method = spec$pupil_filter,
    window = spec$pupil_window
  )
  if (spec$pupil_baseline != "none") {
    x <- baseline_pupil(
      x,
      method = spec$pupil_baseline,
      baseline_window = spec$pupil_baseline_window,
      anchor = "trial_start"
    )
    x <- add_provenance(
      x,
      "workflow_pupil_baseline_notice",
      "eye_samples",
      "Baseline window is relative to media onset and is not necessarily pre-stimulus.",
      warnings = "Do not interpret media-onset baselines as equivalent to a true pre-stimulus baseline."
    )
  }
  x
}

.workflow_trial_features <- function(x) {
  trials <- trial_table(x)
  if (!nrow(trials)) return(empty_eye_table("features"))
  rows <- list()
  k <- 0L
  for (i in seq_len(nrow(trials))) {
    tr <- trials[i, , drop = FALSE]
    gaze <- x$gaze_samples[
      x$gaze_samples$recording_id == tr$recording_id &
        x$gaze_samples$trial_id == tr$trial_id,
      , drop = FALSE
    ]
    pupil <- x$eye_samples[
      x$eye_samples$recording_id == tr$recording_id &
        x$eye_samples$trial_id == tr$trial_id,
      , drop = FALSE
    ]
    fixation <- x$episodes[
      x$episodes$recording_id == tr$recording_id &
        x$episodes$trial_id == tr$trial_id &
        x$episodes$episode_type == "fixation",
      , drop = FALSE
    ]
    values <- c(
      trial_duration_seconds = tr$end_time - tr$start_time,
      gaze_sample_count = nrow(gaze),
      gaze_valid_fraction = if (nrow(gaze)) mean(
        gaze$valid %in% TRUE & is.finite(gaze$gaze_x) & is.finite(gaze$gaze_y)
      ) else NA_real_,
      fixation_count_vendor = sum(fixation$derived_by == "vendor", na.rm = TRUE),
      pupil_observation_count = nrow(pupil),
      pupil_valid_fraction = if (nrow(pupil)) mean(
        pupil$pupil_valid %in% TRUE & is.finite(pupil$pupil_diameter)
      ) else NA_real_
    )
    units <- c("seconds", "count", "proportion", "count", "count", "proportion")
    base <- list(
      recording_id = tr$recording_id,
      participant_id = tr$participant_id,
      trial_id = tr$trial_id,
      item_id = tr$item_id,
      stimulus_id = tr$stimulus_id,
      aoi_id = NA_character_
    )
    k <- k + 1L
    rows[[k]] <- .feature_rows(
      base,
      values,
      units,
      "trial",
      "eyeprocess Gazepoint downstream workflow",
      window_start = tr$start_time,
      window_end = tr$end_time
    )
  }
  do.call(.bind_rows_base, rows)
}

.workflow_biometric_features <- function(x) {
  d <- x$biometrics
  if (!nrow(d)) return(empty_eye_table("features"))
  value_col <- if ("analysis_value" %in% names(d)) "analysis_value" else "value"
  groups <- .group_split(
    d[!is.na(d$trial_id) & nzchar(as.character(d$trial_id)), , drop = FALSE],
    c("recording_id", "trial_id", "channel")
  )
  trials <- trial_table(x)
  rows <- list()
  k <- 0L
  for (z in groups) {
    y <- .safe_numeric(z[[value_col]])
    t <- .safe_numeric(z$timestamp_seconds)
    ok <- is.finite(y) & is.finite(t)
    if (!any(ok)) next
    tr <- trials[
      trials$recording_id == z$recording_id[1L] &
        trials$trial_id == z$trial_id[1L],
      , drop = FALSE
    ]
    if (!nrow(tr)) next
    channel <- as.character(z$channel[1L])
    values <- c(
      mean = mean(y[ok]),
      sd = stats::sd(y[ok]),
      minimum = min(y[ok]),
      maximum = max(y[ok]),
      auc = .trapz(t[ok] - min(t[ok]), y[ok]),
      observed_fraction = mean(ok)
    )
    names(values) <- paste0(channel, "_", names(values))
    unit <- .first_nonmissing(z$unit[ok], "unknown")
    units <- c(unit, unit, unit, unit, paste0(unit, "*seconds"), "proportion")
    base <- list(
      recording_id = tr$recording_id[1L],
      participant_id = tr$participant_id[1L],
      trial_id = tr$trial_id[1L],
      item_id = tr$item_id[1L],
      stimulus_id = tr$stimulus_id[1L],
      aoi_id = NA_character_
    )
    k <- k + 1L
    rows[[k]] <- .feature_rows(
      base,
      values,
      units,
      "trial_channel",
      "eyeprocess Gazepoint downstream workflow",
      parameters = paste0("channel=", channel, ";valid_only=TRUE"),
      window_start = tr$start_time[1L],
      window_end = tr$end_time[1L],
      observed_fraction = mean(ok)
    )
  }
  if (length(rows)) do.call(.bind_rows_base, rows) else empty_eye_table("features")
}

#' Derive the complete Gazepoint workflow feature set
#'
#' @param x An `eye_dataset` with reconstructed media trials.
#' @param reset_workflow_features Remove features previously generated by this
#'   workflow while preserving native Gazepoint Data Summary features.
#' @return The updated `eye_dataset`.
#' @export
derive_gazepoint_workflow_features <- function(x, reset_workflow_features = TRUE) {
  .assert_eye_dataset(x)
  if (!nrow(trial_table(x))) .eye_stop("Media/trial intervals are required.")
  if (isTRUE(reset_workflow_features) && nrow(x$features)) {
    workflow_feature <- grepl(
      "^(derive_gaze_features|derive_pupil_features|derive_rt_features|eyeprocess Gazepoint downstream workflow)",
      x$features$method
    )
    workflow_feature[is.na(workflow_feature)] <- FALSE
    x$features <- x$features[!workflow_feature, , drop = FALSE]
  }
  x <- .workflow_link_features_to_trials(x)
  if (any(x$episodes$episode_type == "fixation")) {
    x <- derive_gaze_features(x, level = "trial", source = "fixations", append = TRUE)
    if (any(!is.na(x$episodes$aoi_id) & nzchar(as.character(x$episodes$aoi_id)))) {
      x <- derive_gaze_features(x, level = "trial_aoi", source = "fixations", append = TRUE)
    }
  } else if (nrow(x$gaze_samples)) {
    x <- derive_gaze_features(x, level = "trial", source = "samples", append = TRUE)
  }
  if (nrow(x$eye_samples)) x <- derive_pupil_features(x, append = TRUE)
  if (nrow(x$responses)) x <- derive_rt_features(x, append = TRUE)
  extra <- .bind_rows_base(
    .workflow_trial_features(x),
    .workflow_biometric_features(x)
  )
  if (nrow(extra)) {
    x$features <- standardize_eye_table(
      .bind_rows_base(x$features, extra),
      "features"
    )
  }
  x <- .workflow_link_features_to_trials(x)
  add_provenance(
    x,
    "derive_gazepoint_workflow_features",
    "features",
    paste0("total feature rows=", nrow(x$features))
  )
}

.workflow_wide_features <- function(features, id_cols) {
  if (!nrow(features)) return(data.frame())
  id_cols <- intersect(id_cols, names(features))
  if (!length(id_cols)) return(data.frame())
  key <- interaction(features[id_cols], drop = TRUE, lex.order = TRUE)
  groups <- split(features, key)
  rows <- lapply(groups, function(z) {
    base <- z[1L, id_cols, drop = FALSE]
    vals <- tapply(z$value, z$feature_name, mean, na.rm = TRUE)
    vals[!is.finite(vals)] <- NA_real_
    cbind(base, as.data.frame(as.list(vals), stringsAsFactors = FALSE))
  })
  out <- do.call(.bind_rows_base, rows)
  rownames(out) <- NULL
  out
}

#' Build person-by-item-by-trial analysis tables
#'
#' @param x A processed `eye_dataset` with workflow features.
#' @return A named list containing trial, AOI, fixation, pupil, biometric,
#'   process, and feature-dictionary tables.
#' @export
gazepoint_analysis_tables <- function(x) {
  .assert_eye_dataset(x)
  trials <- trial_table(x)
  if (!nrow(trials)) .eye_stop("No trial intervals are available.")
  trial_base <- trials[c(
    "recording_id", "participant_id", "trial_id", "item_id",
    "stimulus_id", "condition_id", "start_time", "end_time"
  )]
  trial_base$trial_duration_seconds <- trial_base$end_time - trial_base$start_time
  trial_features <- x$features[
    !is.na(x$features$trial_id) &
      (is.na(x$features$aoi_id) | !nzchar(as.character(x$features$aoi_id))),
    , drop = FALSE
  ]
  wide <- .workflow_wide_features(
    trial_features,
    c("recording_id", "participant_id", "trial_id", "item_id", "stimulus_id")
  )
  if (nrow(wide)) {
    process <- merge(
      trial_base,
      wide,
      by = intersect(names(trial_base), names(wide)),
      all.x = TRUE,
      sort = FALSE
    )
    process <- process[match(trial_base$trial_id, process$trial_id), , drop = FALSE]
  } else {
    process <- trial_base
  }
  fixations <- summarize_fixations(
    x,
    by = c("recording_id", "trial_id"),
    source = "vendor"
  )
  if (nrow(fixations)) {
    fixations <- merge(
      fixations,
      trial_base[c("recording_id", "participant_id", "trial_id", "item_id", "stimulus_id")],
      by = c("recording_id", "trial_id"),
      all.x = TRUE,
      sort = FALSE
    )
  }
  aoi_fixations <- summarize_fixations(
    x,
    by = c("recording_id", "trial_id", "aoi_id"),
    source = "vendor"
  )
  if (is.null(aoi_fixations)) aoi_fixations <- data.frame()
  if (NROW(aoi_fixations) > 0L) {
    aoi_fixations <- merge(
      aoi_fixations,
      trial_base[c("recording_id", "participant_id", "trial_id", "item_id", "stimulus_id")],
      by = c("recording_id", "trial_id"),
      all.x = TRUE,
      sort = FALSE
    )
  }
  aoi_features <- x$features[
    !is.na(x$features$trial_id) &
      !is.na(x$features$aoi_id) &
      nzchar(as.character(x$features$aoi_id)),
    , drop = FALSE
  ]
  aoi_summary <- .workflow_wide_features(
    aoi_features,
    c("recording_id", "participant_id", "trial_id", "item_id", "stimulus_id", "aoi_id")
  )
  if (nrow(aoi_summary) && nrow(x$aoi_definitions)) {
    aoi_summary <- merge(
      aoi_summary,
      unique(x$aoi_definitions[c("aoi_id", "aoi_name")]),
      by = "aoi_id",
      all.x = TRUE,
      sort = FALSE
    )
  }
  if (nrow(aoi_summary) && nrow(aoi_fixations)) {
    aoi_summary <- merge(
      aoi_summary,
      aoi_fixations,
      by = intersect(
        c("recording_id", "participant_id", "trial_id", "item_id", "stimulus_id", "aoi_id"),
        intersect(names(aoi_summary), names(aoi_fixations))
      ),
      all = TRUE,
      sort = FALSE,
      suffixes = c("", "_vendor")
    )
  } else if (!nrow(aoi_summary)) {
    aoi_summary <- aoi_fixations
  }
  pupil <- x$eye_samples[
    !is.na(x$eye_samples$trial_id) & nzchar(as.character(x$eye_samples$trial_id)),
    , drop = FALSE
  ]
  pupil_summary <- data.frame()
  if (nrow(pupil)) {
    groups <- .group_split(pupil, c("recording_id", "trial_id", "eye"))
    pupil_summary <- do.call(rbind, lapply(groups, function(z) {
      ok <- z$pupil_valid %in% TRUE & is.finite(z$pupil_diameter)
      data.frame(
        recording_id = z$recording_id[1L],
        trial_id = z$trial_id[1L],
        eye = z$eye[1L],
        pupil_unit = .first_nonmissing(z$pupil_unit, NA_character_),
        n_observations = nrow(z),
        valid_fraction = mean(ok),
        interpolated_fraction = if ("interpolated" %in% names(z)) mean(z$interpolated, na.rm = TRUE) else 0,
        pupil_mean = if (any(ok)) mean(z$pupil_diameter[ok]) else NA_real_,
        pupil_sd = if (sum(ok) > 1L) stats::sd(z$pupil_diameter[ok]) else NA_real_,
        pupil_min = if (any(ok)) min(z$pupil_diameter[ok]) else NA_real_,
        pupil_max = if (any(ok)) max(z$pupil_diameter[ok]) else NA_real_,
        stringsAsFactors = FALSE
      )
    }))
    pupil_summary <- merge(
      pupil_summary,
      trial_base[c("recording_id", "participant_id", "trial_id", "item_id", "stimulus_id")],
      by = c("recording_id", "trial_id"),
      all.x = TRUE,
      sort = FALSE
    )
  }
  bio <- x$biometrics[
    !is.na(x$biometrics$trial_id) & nzchar(as.character(x$biometrics$trial_id)),
    , drop = FALSE
  ]
  biometric_summary <- data.frame()
  if (nrow(bio)) {
    value_col <- if ("analysis_value" %in% names(bio)) "analysis_value" else "value"
    groups <- .group_split(bio, c("recording_id", "trial_id", "channel"))
    biometric_summary <- do.call(rbind, lapply(groups, function(z) {
      y <- .safe_numeric(z[[value_col]])
      ok <- is.finite(y)
      data.frame(
        recording_id = z$recording_id[1L],
        trial_id = z$trial_id[1L],
        channel = z$channel[1L],
        unit = .first_nonmissing(z$unit, NA_character_),
        n_observations = nrow(z),
        valid_fraction = mean(ok),
        mean = if (any(ok)) mean(y[ok]) else NA_real_,
        sd = if (sum(ok) > 1L) stats::sd(y[ok]) else NA_real_,
        minimum = if (any(ok)) min(y[ok]) else NA_real_,
        maximum = if (any(ok)) max(y[ok]) else NA_real_,
        stringsAsFactors = FALSE
      )
    }))
    biometric_summary <- merge(
      biometric_summary,
      trial_base[c("recording_id", "participant_id", "trial_id", "item_id", "stimulus_id")],
      by = c("recording_id", "trial_id"),
      all.x = TRUE,
      sort = FALSE
    )
  }
  list(
    trials = trial_base,
    process = process,
    fixation_summary = fixations,
    aoi_fixation_summary = aoi_fixations,
    aoi_summary = aoi_summary,
    pupil_summary = pupil_summary,
    biometric_summary = biometric_summary,
    feature_dictionary = feature_dictionary(x)
  )
}

#' Create IRT-ready response and process tables
#'
#' This function does not manufacture responses. When no responses have been
#' supplied, it returns a complete response template and marks the outcome as
#' process-ready but response-pending.
#'
#' @param x A processed `eye_dataset`.
#' @param process_table Optional process table from `gazepoint_analysis_tables()`.
#' @return A list containing a response template, process covariates, long IRT
#'   table, optional matrices, and a readiness assessment.
#' @export
gazepoint_irt_tables <- function(x, process_table = NULL) {
  .assert_eye_dataset(x)
  if (is.null(process_table)) process_table <- gazepoint_analysis_tables(x)$process
  trials <- trial_table(x)
  response_template <- data.frame(
    recording_id = trials$recording_id,
    participant_id = trials$participant_id,
    trial_id = trials$trial_id,
    item_id = trials$item_id,
    stimulus_id = trials$stimulus_id,
    response = NA_character_,
    score = NA_real_,
    response_time = NA_real_,
    stringsAsFactors = FALSE
  )
  if (nrow(x$responses)) {
    r <- x$responses[c(
      "recording_id", "participant_id", "trial_id", "item_id",
      "response", "score", "response_time", "valid_response"
    )]
    response_template <- merge(
      response_template[c("recording_id", "participant_id", "trial_id", "item_id", "stimulus_id")],
      r,
      by = c("recording_id", "participant_id", "trial_id", "item_id"),
      all.x = TRUE,
      sort = FALSE
    )
  }
  irt_long <- merge(
    response_template,
    process_table,
    by = intersect(
      c("recording_id", "participant_id", "trial_id", "item_id", "stimulus_id"),
      intersect(names(response_template), names(process_table))
    ),
    all.x = TRUE,
    sort = FALSE
  )
  n_persons <- length(unique(stats::na.omit(trials$participant_id)))
  n_items <- length(unique(stats::na.omit(trials$item_id)))
  n_trials <- nrow(trials)
  observed_response <- sum(!is.na(response_template$response) & nzchar(as.character(response_template$response)))
  observed_score <- sum(is.finite(.safe_numeric(response_template$score)))
  readiness_status <- if (!observed_score) {
    "process_ready_response_pending"
  } else if (n_persons < 100L || n_items < 5L) {
    "structurally_ready_validation_only"
  } else {
    "model_ready_subject_to_diagnostics"
  }
  readiness <- data.frame(
    metric = c(
      "participants", "items", "trials", "observed_responses",
      "observed_scores", "numeric_process_covariates"
    ),
    value = c(
      n_persons, n_items, n_trials, observed_response, observed_score,
      sum(vapply(process_table, is.numeric, logical(1)))
    ),
    status = c(
      if (n_persons >= 100L) "adequate_candidate" else "limited",
      if (n_items >= 5L) "adequate_candidate" else "limited",
      "available",
      if (observed_response == n_trials) "complete" else "incomplete",
      if (observed_score == n_trials) "complete" else "incomplete",
      "available"
    ),
    stringsAsFactors = FALSE
  )
  response_mat <- NULL
  rt_mat <- NULL
  if (observed_score > 0L) {
    response_mat <- tryCatch(response_matrix(x, value = "score", duplicate = "last"), error = function(e) NULL)
  }
  if (any(is.finite(x$responses$response_time) & x$responses$response_time > 0)) {
    rt_mat <- tryCatch(response_time_matrix(x, duplicate = "last"), error = function(e) NULL)
  }
  list(
    status = readiness_status,
    readiness = readiness,
    response_template = response_template,
    process_covariates = process_table,
    irt_long = irt_long,
    response_matrix = response_mat,
    response_time_matrix = rt_mat,
    guidance = c(
      "No IRT model is fitted automatically.",
      "Provide observed responses and defensible scoring before estimating ability or item parameters.",
      "The six-user demonstration corpus validates software behavior, not psychometric parameter recovery.",
      "Use grouped, person-aware validation and prespecified process covariates in substantive studies."
    )
  )
}

.workflow_qc_tables <- function(x, source_path, spec) {
  list(
    file_pairs = gp_audit_file_pairs(source_path),
    validation = validate_eye_dataset(x),
    readiness = analysis_readiness(x),
    sampling_rate = audit_sampling_rate(
      x,
      expected_hz = spec$expected_sampling_rate,
      tolerance_hz = spec$sampling_tolerance_hz
    ),
    signal_quality = audit_signal_quality(
      x,
      minimum_valid_gaze = spec$minimum_valid_gaze,
      minimum_valid_pupil = spec$minimum_valid_pupil,
      by_trial = TRUE
    ),
    pupil_quality = audit_pupil_quality(x),
    trial_coverage = audit_trial_coverage(x),
    episodes = audit_episodes(x),
    clock_sync = audit_clock_sync(x),
    coordinate_spaces = audit_coordinate_spaces(x),
    aoi = audit_aois(x),
    gaze_missingness = audit_missingness(x, "gaze_samples"),
    pupil_missingness = audit_missingness(x, "eye_samples"),
    biometric_missingness = audit_missingness(x, "biometrics")
  )
}

.workflow_save_plot <- function(path, fun, width = 1400, height = 900, res = 130) {
  status <- tryCatch({
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    .save_plot(path, fun, width = width, height = height, res = res)
    data.frame(
      plot = basename(path), path = normalizePath(path, winslash = "/", mustWork = FALSE),
      status = "created", message = "", stringsAsFactors = FALSE
    )
  }, error = function(e) {
    data.frame(
      plot = basename(path), path = normalizePath(path, winslash = "/", mustWork = FALSE),
      status = "failed", message = conditionMessage(e), stringsAsFactors = FALSE
    )
  })
  status
}

#' Generate the complete Gazepoint workflow plot suite
#'
#' @param x A processed `eye_dataset`.
#' @param directory Destination directory.
#' @param channels Biometric channels to plot.
#' @param expected_hz Expected gaze sampling rate shown in diagnostics.
#' @return A plot manifest data frame.
#' @export
plot_gazepoint_workflow <- function(x, directory, channels = NULL, expected_hz = 60) {
  .assert_eye_dataset(x)
  directory <- normalizePath(directory, winslash = "/", mustWork = FALSE)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  manifests <- list()
  add <- function(name, fun, subdir = "summary", width = 1400, height = 900) {
    path <- file.path(directory, subdir, name)
    manifests[[length(manifests) + 1L]] <<- .workflow_save_plot(path, fun, width, height)
  }
  add("dataset-overview.png", function() plot_eye_overview(x))
  add("sampling-rate.png", function() plot_sampling_rate(x, expected_hz = expected_hz))
  add("signal-quality.png", function() plot_signal_quality(x, by_trial = TRUE), height = 1200)
  add("trial-timeline.png", function() plot_trial_timeline(x), height = 1200)
  add("coordinate-spaces.png", function() plot_coordinate_spaces(x))
  add("gaze-missingness.png", function() plot_missingness(x, "gaze_samples"), height = 1100)
  if (nrow(x$eye_samples)) add("pupil-missingness.png", function() plot_missingness(x, "eye_samples"), height = 1100)
  if (nrow(x$biometrics)) {
    add("biometric-missingness.png", function() plot_missingness(x, "biometrics"), height = 1100)
    add("clock-alignment.png", function() plot_clock_alignment(x))
  }
  if (any(x$features$feature_name == "dwell_time_ms" & !is.na(x$features$aoi_id))) {
    add("aoi-dwell.png", function() plot_aoi_dwell(x, "dwell_time_ms"), height = 1100)
  }
  numeric_features <- gazepoint_analysis_tables(x)$process
  if (sum(vapply(numeric_features, is.numeric, logical(1))) >= 2L) {
    add("feature-correlations.png", function() plot_feature_correlation(x), height = 1200)
  }
  trials <- trial_table(x)
  for (i in seq_len(nrow(trials))) {
    rec <- trials$recording_id[i]
    trial <- trials$trial_id[i]
    stem <- paste(.workflow_token(rec), .workflow_token(trial), sep = "__")
    add(
      paste0(stem, "__gaze-trace.png"),
      function(rec = rec, trial = trial) plot_eye_trace(
        x, recording_id = rec, trial_id = trial,
        main = paste("Gaze trace:", rec, trial)
      ),
      "gaze"
    )
    add(
      paste0(stem, "__gaze-heatmap.png"),
      function(rec = rec, trial = trial) plot_gaze_heatmap(
        x, recording_id = rec, trial_id = trial,
        main = paste("Gaze density:", rec, trial)
      ),
      "gaze"
    )
    add(
      paste0(stem, "__fixations.png"),
      function(rec = rec, trial = trial) plot_fixations(
        x, recording_id = rec, trial_id = trial, source = "vendor",
        main = paste("Vendor fixations:", rec, trial)
      ),
      "fixations"
    )
    add(
      paste0(stem, "__scanpath.png"),
      function(rec = rec, trial = trial) plot_scanpath(
        x, recording_id = rec, trial_id = trial,
        main = paste("Scanpath:", rec, trial)
      ),
      "fixations"
    )
    if (any(x$eye_samples$recording_id == rec & x$eye_samples$trial_id == trial)) {
      add(
        paste0(stem, "__pupil.png"),
        function(rec = rec, trial = trial) plot_pupil_timeseries(
          x, recording_id = rec, trial_id = trial,
          main = paste("Pupil time series:", rec, trial)
        ),
        "pupil"
      )
    }
  }
  if (nrow(x$biometrics)) {
    selected <- unique(x$biometrics$channel)
    if (!is.null(channels)) selected <- intersect(selected, channels)
    plot_data <- x
    if ("analysis_value" %in% names(plot_data$biometrics)) {
      plot_data$biometrics$value <- plot_data$biometrics$analysis_value
    } else {
      plot_data$biometrics$value[!(plot_data$biometrics$valid %in% TRUE)] <- NA_real_
    }
    for (rec in unique(x$recordings$recording_id)) {
      available <- unique(
        plot_data$biometrics$channel[
          plot_data$biometrics$recording_id == rec &
            is.finite(plot_data$biometrics$value)
        ]
      )
      available <- intersect(selected, available)
      if (!length(available)) next
      add(
        paste0(.workflow_token(rec), "__biometrics.png"),
        function(rec = rec, available = available) plot_biometrics(
          plot_data,
          channels = available,
          recording_id = rec,
          main = paste("Valid biometrics:", rec)
        ),
        "biometrics",
        height = max(900, 330 * length(available))
      )
    }
  }
  manifest <- if (length(manifests)) do.call(rbind, manifests) else data.frame()
  rownames(manifest) <- NULL
  manifest
}

.workflow_matrix_table <- function(x, row_name = "participant_id") {
  if (is.null(x)) return(data.frame())
  out <- data.frame(x, check.names = FALSE, stringsAsFactors = FALSE)
  out[[row_name]] <- rownames(x)
  out <- out[c(row_name, setdiff(names(out), row_name))]
  rownames(out) <- NULL
  out
}

.workflow_write_tables <- function(tables, irt, qc, directory) {
  paths <- list()
  table_dir <- file.path(directory, "tables")
  qc_dir <- file.path(directory, "qc")
  irt_dir <- file.path(directory, "irt")
  dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(qc_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(irt_dir, recursive = TRUE, showWarnings = FALSE)
  for (nm in names(tables)) {
    if (!is.data.frame(tables[[nm]])) next
    paths[[paste0("table_", nm)]] <- .workflow_write_csv(
      tables[[nm]],
      file.path(table_dir, paste0(gsub("_", "-", nm), ".csv"))
    )
  }
  for (nm in names(qc)) {
    if (!is.data.frame(qc[[nm]])) next
    paths[[paste0("qc_", nm)]] <- .workflow_write_csv(
      qc[[nm]],
      file.path(qc_dir, paste0(gsub("_", "-", nm), ".csv"))
    )
  }
  paths$irt_readiness <- .workflow_write_csv(
    irt$readiness,
    file.path(irt_dir, "irt-readiness.csv")
  )
  paths$irt_response_template <- .workflow_write_csv(
    irt$response_template,
    file.path(irt_dir, "response-template.csv")
  )
  paths$irt_process_covariates <- .workflow_write_csv(
    irt$process_covariates,
    file.path(irt_dir, "process-covariates.csv")
  )
  paths$irt_long <- .workflow_write_csv(
    irt$irt_long,
    file.path(irt_dir, "irt-long.csv")
  )
  if (!is.null(irt$response_matrix)) {
    paths$irt_response_matrix <- .workflow_write_csv(
      .workflow_matrix_table(irt$response_matrix),
      file.path(irt_dir, "response-matrix.csv")
    )
  }
  if (!is.null(irt$response_time_matrix)) {
    paths$irt_response_time_matrix <- .workflow_write_csv(
      .workflow_matrix_table(irt$response_time_matrix),
      file.path(irt_dir, "response-time-matrix.csv")
    )
  }
  paths
}

.workflow_relative <- function(path, root) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  prefix <- paste0(root, "/")
  if (startsWith(path, prefix)) substring(path, nchar(prefix) + 1L) else path
}

.workflow_r_quote <- function(x) encodeString(as.character(x), quote = '"')


#' Write a reproducible Gazepoint workflow report
#'
#' @param workflow An `eye_gazepoint_workflow` result.
#' @param path Markdown report destination.
#' @param render_html Render an HTML copy when possible.
#' @return The normalized report path.
#' @export
write_gazepoint_workflow_report <- function(
    workflow,
    path = file.path(workflow$output_dir, "gazepoint-workflow-report.md"),
    render_html = workflow$spec$create_html_report) {
  if (!inherits(workflow, "eye_gazepoint_workflow")) {
    .eye_stop("`workflow` must be returned by `run_gazepoint_workflow()`.")
  }
  x <- workflow$dataset
  counts <- c(
    recordings = nrow(x$recordings),
    participants = length(unique(stats::na.omit(x$recordings$participant_id))),
    trials = nrow(trial_table(x)),
    gaze_samples = nrow(x$gaze_samples),
    fixations = sum(x$episodes$episode_type == "fixation"),
    eye_samples = nrow(x$eye_samples),
    biometrics = nrow(x$biometrics),
    aois = nrow(x$aoi_definitions),
    features = nrow(x$features),
    responses = nrow(x$responses)
  )
  plot_rows <- workflow$plot_manifest
  plot_lines <- character()
  if (is.data.frame(plot_rows) && nrow(plot_rows)) {
    created <- plot_rows[plot_rows$status == "created", , drop = FALSE]
    plot_lines <- unlist(lapply(seq_len(nrow(created)), function(i) {
      rel <- .workflow_relative(created$path[i], dirname(path))
      c(paste0("### ", tools::file_path_sans_ext(created$plot[i])), "", paste0("![](", rel, ")"), "")
    }))
  }
  lines <- c(
    "# Gazepoint downstream workflow report", "",
    paste0("Generated: ", .now_utc()), "",
    "## Workflow status", "",
    paste0("- Status: **", toupper(workflow$status), "**"),
    paste0("- Source: `", workflow$source_path, "`"),
    paste0("- Output: `", workflow$output_dir, "`"),
    paste0("- IRT readiness: `", workflow$irt$status, "`"), "",
    "## Canonical dataset", "",
    .markdown_table(data.frame(metric = names(counts), value = as.numeric(counts), stringsAsFactors = FALSE)), "",
    "## Media/trial reconstruction", "",
    .markdown_table(workflow$tables$trials), "",
    "Media presentations are treated as analysis trials. By default, `item_id` equals the Gazepoint `stimulus_id`; supply an item map to replace these labels.", "",
    "## QC and recording diagnostics", "",
    "### File pairing", "",
    .markdown_table(workflow$qc$file_pairs), "",
    "### Analysis readiness", "",
    .markdown_table(workflow$qc$readiness), "",
    "### Sampling rate", "",
    .markdown_table(workflow$qc$sampling_rate), "",
    "### Signal quality", "",
    .markdown_table(workflow$qc$signal_quality), "",
    "### Trial coverage", "",
    .markdown_table(workflow$qc$trial_coverage), "",
    "### Pupil quality", "",
    .markdown_table(workflow$qc$pupil_quality), "",
    "### Ocular episodes", "",
    .markdown_table(workflow$qc$episodes), "",
    "### Clock synchronization", "",
    .markdown_table(workflow$qc$clock_sync), "",
    "### Coordinate spaces", "",
    .markdown_table(workflow$qc$coordinate_spaces), "",
    "### AOI registry", "",
    .markdown_table(workflow$qc$aoi), "",
    if (is.data.frame(workflow$workflow_checks)) c(
      "### Workflow validation", "",
      .markdown_table(workflow$workflow_checks), ""
    ) else character(),
    "## Fixation and AOI summaries", "",
    paste0("- Vendor trial-level fixation rows: ", nrow(workflow$tables$fixation_summary)),
    paste0("- Vendor AOI-fixation rows: ", nrow(workflow$tables$aoi_fixation_summary)),
    paste0("- Combined AOI summary rows: ", nrow(workflow$tables$aoi_summary)), "",
    "AOI summary exports can support AOI-level measures without supplying dynamic geometry. Sample-level AOI assignment requires compatible AOI geometry.", "",
    "## Pupil and biometrics", "",
    paste0("- Pupil summary rows: ", nrow(workflow$tables$pupil_summary)),
    paste0("- Biometric summary rows: ", nrow(workflow$tables$biometric_summary)), "",
    "Only observations with valid vendor flags enter biometric analysis features. Native values remain preserved in the canonical object.", "",
    "## Analysis-ready process table", "",
    paste0("The person-by-item-by-trial process table contains ", nrow(workflow$tables$process), " rows and ", ncol(workflow$tables$process), " columns."), "",
    "## IRT-ready outputs", "",
    .markdown_table(workflow$irt$readiness), "",
    paste0("Status: `", workflow$irt$status, "`."), "",
    paste0("- ", workflow$irt$guidance), "",
    "## Responsible interpretation", "",
    .markdown_table(interpretive_warnings()), "",
    "## Reproducibility", "",
    "The output directory contains the canonical dataset, QC tables, process tables, IRT templates, plot manifest, workflow specification, session information, source fingerprint, and a rerun script.", "",
    if (length(plot_lines)) c("## Plots", "", plot_lines) else character()
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path, useBytes = TRUE)
  if (isTRUE(render_html) && requireNamespace("rmarkdown", quietly = TRUE)) {
    rmd <- paste0(tools::file_path_sans_ext(path), ".Rmd")
    wrapper <- c(
      "---",
      "title: \"Gazepoint downstream workflow report\"",
      "output:",
      "  html_document:",
      "    toc: true",
      "    toc_float: true",
      "---", "",
      "```{r, echo=FALSE, results='asis'}",
      paste0("cat(readLines(", .workflow_r_quote(normalizePath(path, winslash = "/", mustWork = FALSE)), "), sep='\\n')"),
      "```"
    )
    writeLines(wrapper, rmd, useBytes = TRUE)
    try(
      rmarkdown::render(
        rmd,
        output_file = paste0(tools::file_path_sans_ext(basename(path)), ".html"),
        output_dir = dirname(path),
        quiet = TRUE,
        envir = new.env(parent = baseenv())
      ),
      silent = TRUE
    )
  }
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

.workflow_reproducibility_files <- function(workflow) {
  root <- workflow$output_dir
  saveRDS(workflow$spec, file.path(root, "workflow-spec.rds"))
  saveRDS(workflow, file.path(root, "workflow-result.rds"))
  capture.output(sessionInfo(), file = file.path(root, "session-info.txt"))
  source_fingerprint <- data.frame(
    file = list.files(workflow$source_path, recursive = TRUE, full.names = TRUE),
    stringsAsFactors = FALSE
  )
  source_fingerprint <- source_fingerprint[file.exists(source_fingerprint$file), , drop = FALSE]
  source_fingerprint$size_bytes <- file.info(source_fingerprint$file)$size
  source_fingerprint$md5 <- unname(tools::md5sum(source_fingerprint$file))
  source_fingerprint$file <- vapply(
    source_fingerprint$file,
    .workflow_relative,
    character(1),
    root = workflow$source_path
  )
  .workflow_write_csv(source_fingerprint, file.path(root, "source-fingerprint.csv"))
  item_map_path <- file.path(root, "item-map.csv")
  if (is.data.frame(workflow$item_map)) {
    .workflow_write_csv(workflow$item_map, item_map_path)
  }
  responses_path <- file.path(root, "responses-supplied.csv")
  if (isTRUE(workflow$responses_supplied) && nrow(workflow$dataset$responses)) {
    .workflow_write_csv(workflow$dataset$responses, responses_path)
  }
  rerun <- c(
    "library(eyeprocess)",
    "",
    paste0("source_path <- ", .workflow_r_quote(workflow$source_path)),
    paste0("output_dir <- ", .workflow_r_quote(workflow$output_dir)),
    "spec <- readRDS(file.path(output_dir, 'workflow-spec.rds'))",
    "item_map_path <- file.path(output_dir, 'item-map.csv')",
    "responses_path <- file.path(output_dir, 'responses-supplied.csv')",
    "item_map <- if (file.exists(item_map_path)) utils::read.csv(item_map_path, stringsAsFactors = FALSE) else NULL",
    "responses <- if (file.exists(responses_path)) utils::read.csv(responses_path, stringsAsFactors = FALSE) else NULL",
    "",
    "result <- run_gazepoint_workflow(",
    "  source_path,",
    "  output_dir = output_dir,",
    "  responses = responses,",
    "  item_map = item_map,",
    "  spec = spec,",
    "  overwrite = TRUE",
    ")"
  )
  writeLines(rerun, file.path(root, "rerun-workflow.R"), useBytes = TRUE)
  invisible(root)
}

#' Run the complete Gazepoint downstream workflow
#'
#' Imports a real Gazepoint folder, constructs a canonical `eye_dataset`, runs
#' QC, reconstructs media trials, processes pupil and biometric streams,
#' derives gaze/AOI/pupil/biometric features, creates analysis and IRT tables,
#' generates plots, exports every stage, and writes a reproducible report.
#'
#' @param path Gazepoint export folder.
#' @param output_dir Destination directory.
#' @param responses Optional response data frame or CSV path.
#' @param score_key Optional named vector of correct responses by item id.
#' @param item_map Optional stimulus-to-item map.
#' @param spec Workflow specification from `gazepoint_workflow_spec()`.
#' @param overwrite Replace an existing output directory.
#' @param quiet Suppress progress messages.
#' @return An `eye_gazepoint_workflow` object.
#' @export
run_gazepoint_workflow <- function(
    path,
    output_dir = file.path(getwd(), "eyeprocess-gazepoint-workflow"),
    responses = NULL,
    score_key = NULL,
    item_map = NULL,
    spec = gazepoint_workflow_spec(),
    overwrite = FALSE,
    quiet = FALSE) {
  if (!inherits(spec, "eye_gazepoint_workflow_spec")) {
    .eye_stop("`spec` must be created with `gazepoint_workflow_spec()`.")
  }
  if (!dir.exists(path)) .eye_stop("Gazepoint source directory does not exist: ", path)
  source_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  output_dir <- .workflow_clean_output(output_dir, overwrite)
  .eye_message("[1/10] Importing Gazepoint folder", quiet = quiet)
  x <- read_gazepoint_folder(
    source_path,
    include = c("gaze", "fixations", "events", "biometrics", "aoi"),
    keep_raw = spec$retain_raw,
    quiet = quiet
  )
  resolved_item_map <- .workflow_item_map(item_map, x$gaze_samples$stimulus_id)
  .eye_message("[2/10] Reconstructing media trials", quiet = quiet)
  x <- build_gazepoint_media_trials(x, item_map = resolved_item_map, overwrite = TRUE)
  .eye_message("[3/10] Adding optional response data", quiet = quiet)
  response_result <- .workflow_prepare_responses(x, responses, score_key)
  x <- response_result$dataset
  .eye_message("[4/10] Processing pupil and biometric streams", quiet = quiet)
  x <- .workflow_preprocess_pupil(x, spec)
  x <- .workflow_prepare_biometrics(x)
  .eye_message("[5/10] Deriving trial and AOI features", quiet = quiet)
  x <- derive_gazepoint_workflow_features(x, reset_workflow_features = TRUE)
  .eye_message("[6/10] Computing QC evidence", quiet = quiet)
  qc <- .workflow_qc_tables(x, source_path, spec)
  validation_errors <- if (nrow(qc$validation)) sum(qc$validation$severity == "error") else 0L
  status <- if (validation_errors > 0L) "fail" else "pass"
  .eye_message("[7/10] Building analysis and IRT tables", quiet = quiet)
  tables <- gazepoint_analysis_tables(x)
  irt <- gazepoint_irt_tables(x, tables$process)
  canonical_dir <- file.path(output_dir, "canonical-dataset")
  write_eye_dataset(
    x,
    canonical_dir,
    include_raw = spec$retain_raw,
    overwrite = TRUE,
    manifest = TRUE
  )
  paths <- .workflow_write_tables(tables, irt, qc, output_dir)
  paths$canonical_dataset <- normalizePath(canonical_dir, winslash = "/", mustWork = FALSE)
  paths$canonical_report <- report_eye_dataset(
    x,
    path = file.path(output_dir, "canonical-validation-report.md"),
    title = "eyeprocess canonical Gazepoint validation report",
    include_plots = FALSE
  )
  .eye_message("[8/10] Generating plots", quiet = quiet)
  plot_manifest <- if (isTRUE(spec$create_plots)) {
    plot_gazepoint_workflow(
      x,
      file.path(output_dir, "plots"),
      channels = spec$biometric_channels,
      expected_hz = spec$expected_sampling_rate
    )
  } else {
    data.frame()
  }
  if (nrow(plot_manifest)) {
    paths$plot_manifest <- .workflow_write_csv(
      plot_manifest,
      file.path(output_dir, "plots", "plot-manifest.csv")
    )
  }
  result <- structure(
    list(
      status = status,
      source_path = source_path,
      output_dir = output_dir,
      dataset = x,
      qc = qc,
      tables = tables,
      irt = irt,
      response_template = response_result$response_template,
      responses_supplied = response_result$supplied,
      item_map = resolved_item_map,
      plot_manifest = plot_manifest,
      paths = paths,
      spec = spec,
      created_at = .now_utc()
    ),
    class = "eye_gazepoint_workflow"
  )
  .eye_message("[9/10] Writing reproducibility assets", quiet = quiet)
  .workflow_reproducibility_files(result)
  .eye_message("[10/10] Writing report", quiet = quiet)
  result$paths$report <- write_gazepoint_workflow_report(result, render_html = FALSE)
  workflow_checks <- validate_gazepoint_workflow(result)
  result$workflow_checks <- workflow_checks
  result$paths$workflow_validation <- .workflow_write_csv(
    workflow_checks,
    file.path(output_dir, "workflow-validation.csv")
  )
  if (any(!workflow_checks$passed)) result$status <- "fail"
  result$paths$report <- write_gazepoint_workflow_report(result)
  saveRDS(result, file.path(output_dir, "workflow-result.rds"))
  if (result$status == "fail") {
    .eye_warn("Workflow completed, but canonical validation contains errors. Review the QC output.")
  } else {
    .eye_message("Gazepoint downstream workflow completed: ", output_dir, quiet = quiet)
  }
  result
}

#' @export
print.eye_gazepoint_workflow <- function(x, ...) {
  cat("<eye_gazepoint_workflow>\n")
  cat("  Status:       ", toupper(x$status), "\n", sep = "")
  cat("  Recordings:   ", nrow(x$dataset$recordings), "\n", sep = "")
  cat("  Participants: ", length(unique(stats::na.omit(x$dataset$recordings$participant_id))), "\n", sep = "")
  cat("  Trials:       ", nrow(trial_table(x$dataset)), "\n", sep = "")
  cat("  Process rows: ", nrow(x$tables$process), "\n", sep = "")
  cat("  IRT status:   ", x$irt$status, "\n", sep = "")
  cat("  Output:       ", x$output_dir, "\n", sep = "")
  invisible(x)
}

#' @export
summary.eye_gazepoint_workflow <- function(object, ...) {
  data.frame(
    status = object$status,
    recordings = nrow(object$dataset$recordings),
    participants = length(unique(stats::na.omit(object$dataset$recordings$participant_id))),
    trials = nrow(trial_table(object$dataset)),
    gaze_samples = nrow(object$dataset$gaze_samples),
    fixations = sum(object$dataset$episodes$episode_type == "fixation"),
    eye_samples = nrow(object$dataset$eye_samples),
    biometrics = nrow(object$dataset$biometrics),
    process_rows = nrow(object$tables$process),
    irt_status = object$irt$status,
    stringsAsFactors = FALSE
  )
}

#' Validate an integrated Gazepoint workflow result
#'
#' @param x An `eye_gazepoint_workflow` object.
#' @return A data frame of workflow checks.
#' @export
validate_gazepoint_workflow <- function(x) {
  if (!inherits(x, "eye_gazepoint_workflow")) {
    .eye_stop("`x` must be returned by `run_gazepoint_workflow()`.")
  }
  trial_keys <- paste(
    x$tables$trials$recording_id,
    x$tables$trials$trial_id,
    sep = "\r"
  )
  process_keys <- paste(
    x$tables$process$recording_id,
    x$tables$process$trial_id,
    sep = "\r"
  )
  required_paths <- c(
    x$paths$canonical_dataset,
    x$paths$report,
    file.path(x$output_dir, "workflow-result.rds"),
    file.path(x$output_dir, "workflow-spec.rds"),
    file.path(x$output_dir, "rerun-workflow.R")
  )
  checks <- data.frame(
    check = c(
      "canonical_validation", "unique_trial_keys", "process_row_per_trial",
      "native_time_preserved", "coordinate_space_registered",
      "provenance_available", "required_outputs_exist"
    ),
    passed = c(
      !nrow(x$qc$validation) || !any(x$qc$validation$severity == "error"),
      anyDuplicated(trial_keys) == 0L,
      anyDuplicated(process_keys) == 0L && nrow(x$tables$process) == nrow(x$tables$trials),
      nrow(x$dataset$gaze_samples) > 0L && any(is.finite(x$dataset$gaze_samples$timestamp_native)),
      all(stats::na.omit(unique(x$dataset$gaze_samples$coordinate_space_id)) %in% x$dataset$coordinate_spaces$coordinate_space_id),
      nrow(x$dataset$provenance) > 0L,
      all(file.exists(required_paths) | dir.exists(required_paths))
    ),
    stringsAsFactors = FALSE
  )
  checks$status <- ifelse(checks$passed, "pass", "fail")
  checks
}
