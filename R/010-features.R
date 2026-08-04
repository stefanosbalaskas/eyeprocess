feature_spec <- function(
    level = c("trial", "trial_aoi", "recording", "participant_item"),
    window = NULL,
    include_post_response = FALSE,
    minimum_observed_fraction = 0.5,
    gaze = c("fixation_count", "fixation_duration", "dwell_time", "first_fixation_latency", "revisits", "entropy"),
    pupil = c("mean", "peak", "auc", "slope", "latency_peak"),
    response_time = TRUE,
    biometrics = TRUE) {
  level <- match.arg(level)
  structure(list(
    level = level, window = window,
    include_post_response = isTRUE(include_post_response),
    minimum_observed_fraction = as.numeric(minimum_observed_fraction),
    gaze = gaze, pupil = pupil,
    response_time = isTRUE(response_time), biometrics = isTRUE(biometrics)
  ), class = "eye_feature_spec")
}

print.eye_feature_spec <- function(x, ...) {
  cat("<eye_feature_spec>\n")
  cat("  Level: ", x$level, "\n", sep = "")
  cat("  Gaze:  ", paste(x$gaze, collapse = ", "), "\n", sep = "")
  cat("  Pupil: ", paste(x$pupil, collapse = ", "), "\n", sep = "")
  invisible(x)
}

.feature_rows <- function(base, values, units, level, method, parameters = NA_character_, window_start = NA_real_, window_end = NA_real_, observed_fraction = NA_real_) {
  if (!length(values)) return(empty_eye_table("features"))
  if (length(units) == 1L) units <- rep(units, length(values))
  data.frame(
    feature_id = .next_id("feature", length(values)),
    recording_id = base$recording_id %||% NA_character_,
    participant_id = base$participant_id %||% NA_character_,
    trial_id = base$trial_id %||% NA_character_,
    item_id = base$item_id %||% NA_character_,
    stimulus_id = base$stimulus_id %||% NA_character_,
    aoi_id = base$aoi_id %||% NA_character_,
    feature_name = names(values), value = as.numeric(values), unit = units,
    level = level, window_start = window_start, window_end = window_end,
    observed_fraction = observed_fraction, method = method,
    parameters = parameters, derived_at = .now_utc(), stringsAsFactors = FALSE
  )
}

trial_table <- function(x) {
  .assert_eye_dataset(x)
  x$intervals[x$intervals$interval_type == "trial", , drop = FALSE]
}

summarize_fixations <- function(x, by = c("recording_id", "trial_id", "aoi_id"), source = c("all", "vendor", "eyeprocess")) {
  .assert_eye_dataset(x)
  source <- match.arg(source)
  d <- x$episodes[x$episodes$episode_type == "fixation", , drop = FALSE]
  if (source == "vendor") d <- d[d$derived_by == "vendor", ]
  if (source == "eyeprocess") d <- d[d$derived_by == "eyeprocess", ]
  by <- intersect(by, names(d))
  if (!nrow(d) || !length(by)) return(data.frame())
  groups <- .group_split(d, by)
  out <- lapply(groups, function(z) {
    base <- z[1L, by, drop = FALSE]
    cbind(base, data.frame(
      fixation_count = nrow(z),
      fixation_duration_total_ms = sum(z$duration_ms, na.rm = TRUE),
      fixation_duration_mean_ms = mean(z$duration_ms, na.rm = TRUE),
      fixation_duration_median_ms = stats::median(z$duration_ms, na.rm = TRUE),
      first_fixation_time = min(z$start_time, na.rm = TRUE),
      last_fixation_time = max(z$end_time, na.rm = TRUE),
      stringsAsFactors = FALSE
    ))
  })
  do.call(rbind, out)
}

scanpath_sequence <- function(x, trial_id = NULL, recording_id = NULL, source = c("visits", "fixations", "samples"), collapse_consecutive = TRUE) {
  .assert_eye_dataset(x)
  source <- match.arg(source)
  if (source == "samples") {
    d <- x$gaze_samples
    if (!"aoi_id" %in% names(d)) .eye_stop("AOIs have not been assigned to gaze samples.")
    d <- data.frame(recording_id = d$recording_id, trial_id = d$trial_id, time = d$timestamp_seconds, aoi_id = d$aoi_id, stringsAsFactors = FALSE)
  } else {
    type <- if (source == "visits") "aoi_visit" else "fixation"
    d <- x$episodes[x$episodes$episode_type == type, c("recording_id", "trial_id", "start_time", "aoi_id"), drop = FALSE]
    names(d)[names(d) == "start_time"] <- "time"
  }
  if (!is.null(trial_id)) d <- d[d$trial_id %in% trial_id, ]
  if (!is.null(recording_id)) d <- d[d$recording_id %in% recording_id, ]
  d <- d[!is.na(d$aoi_id), ]; d <- d[order(d$recording_id, d$trial_id, d$time), ]
  groups <- split(d, interaction(d$recording_id, d$trial_id, drop = TRUE))
  out <- lapply(groups, function(z) {
    seq <- as.character(z$aoi_id)
    if (collapse_consecutive && length(seq)) seq <- seq[c(TRUE, seq[-1L] != seq[-length(seq)])]
    data.frame(
      recording_id = z$recording_id[1L], trial_id = z$trial_id[1L],
      sequence = paste(seq, collapse = " > "), length = length(seq),
      stringsAsFactors = FALSE
    )
  })
  if (!length(out)) return(data.frame())
  do.call(rbind, out)
}

transition_matrix <- function(x, normalize = c("none", "row", "all"), source = c("visits", "fixations", "samples"), include_self = FALSE) {
  .assert_eye_dataset(x)
  normalize <- match.arg(normalize); source <- match.arg(source)
  seqs <- scanpath_sequence(x, source = source, collapse_consecutive = !include_self)
  if (!nrow(seqs)) return(matrix(0, 0, 0))
  all_trans <- list()
  for (s in seqs$sequence) {
    z <- strsplit(s, " > ", fixed = TRUE)[[1L]]
    if (length(z) < 2L) next
    all_trans[[length(all_trans) + 1L]] <- data.frame(from = head(z, -1L), to = tail(z, -1L), stringsAsFactors = FALSE)
  }
  if (!length(all_trans)) return(matrix(0, 0, 0))
  tr <- do.call(rbind, all_trans)
  if (!include_self) tr <- tr[tr$from != tr$to, ]
  levels <- sort(unique(c(tr$from, tr$to)))
  mat <- table(factor(tr$from, levels), factor(tr$to, levels))
  mat <- as.matrix(mat)
  if (normalize == "row") {
    rs <- rowSums(mat); mat <- sweep(mat, 1L, ifelse(rs == 0, 1, rs), "/")
  }
  if (normalize == "all") mat <- mat / sum(mat)
  mat
}

gaze_entropy <- function(x, level = c("trial", "recording"), source = c("visits", "fixations", "samples"), base = 2) {
  .assert_eye_dataset(x)
  level <- match.arg(level); source <- match.arg(source)
  if (source == "samples") {
    d <- x$gaze_samples
    if (!"aoi_id" %in% names(d)) .eye_stop("AOIs have not been assigned.")
  } else {
    type <- if (source == "visits") "aoi_visit" else "fixation"
    d <- x$episodes[x$episodes$episode_type == type, ]
  }
  d <- d[!is.na(d$aoi_id), ]
  keys <- if (level == "trial") c("recording_id", "trial_id") else "recording_id"
  groups <- .group_split(d, keys)
  out <- lapply(groups, function(z) {
    p <- prop.table(table(z$aoi_id))
    h <- -sum(p * log(p, base = base))
    base_row <- z[1L, keys, drop = FALSE]
    cbind(base_row, data.frame(entropy = h, n_states = length(p), stringsAsFactors = FALSE))
  })
  if (!length(out)) return(data.frame())
  do.call(rbind, out)
}

transition_entropy <- function(x, source = c("visits", "fixations", "samples"), base = 2) {
  source <- match.arg(source)
  mat <- transition_matrix(x, normalize = "row", source = source)
  if (!length(mat)) return(data.frame())
  h <- apply(mat, 1L, function(p) {
    p <- p[p > 0]
    if (!length(p)) 0 else -sum(p * log(p, base = base))
  })
  data.frame(aoi_id = rownames(mat), transition_entropy = h, stringsAsFactors = FALSE)
}

derive_gaze_features <- function(x, level = c("trial_aoi", "trial"), source = c("fixations", "visits", "samples"), append = TRUE) {
  .assert_eye_dataset(x)
  level <- match.arg(level); source <- match.arg(source)
  trials <- trial_table(x)
  if (!nrow(trials)) .eye_stop("Trial intervals are required for trial-level gaze features.")
  if (source == "fixations") d <- x$episodes[x$episodes$episode_type == "fixation", ]
  if (source == "visits") d <- x$episodes[x$episodes$episode_type == "aoi_visit", ]
  if (source == "samples") {
    d <- x$gaze_samples
    if (!"aoi_id" %in% names(d)) d$aoi_id <- NA_character_
    d$start_time <- d$timestamp_seconds; d$end_time <- d$timestamp_seconds
    hz_map <- setNames(x$streams$observed_rate_hz[x$streams$stream_type == "gaze_combined"], x$streams$recording_id[x$streams$stream_type == "gaze_combined"])
    d$duration_ms <- 1000 / hz_map[d$recording_id]
  }
  if (!nrow(d)) return(x)
  keys <- c("recording_id", "trial_id")
  if (level == "trial_aoi") keys <- c(keys, "aoi_id")
  groups <- .group_split(d[!is.na(d$trial_id), ], keys)
  rows <- list(); k <- 0L
  for (z in groups) {
    rec <- z$recording_id[1L]; trial <- z$trial_id[1L]
    tr <- trials[trials$recording_id == rec & trials$trial_id == trial, ]
    if (!nrow(tr)) next
    trial_duration <- (tr$end_time[1L] - tr$start_time[1L]) * 1000
    seq_aoi <- as.character(z$aoi_id[order(z$start_time)])
    seq_aoi <- seq_aoi[!is.na(seq_aoi)]
    revisits <- if (length(seq_aoi)) sum(seq_aoi[-1L] != seq_aoi[-length(seq_aoi)]) else 0
    p <- prop.table(table(seq_aoi)); entropy <- if (length(p)) -sum(p * log2(p)) else NA_real_
    vals <- c(
      fixation_count = nrow(z),
      fixation_duration_total_ms = sum(z$duration_ms, na.rm = TRUE),
      fixation_duration_mean_ms = mean(z$duration_ms, na.rm = TRUE),
      fixation_duration_median_ms = stats::median(z$duration_ms, na.rm = TRUE),
      dwell_time_ms = sum(z$duration_ms, na.rm = TRUE),
      dwell_proportion = sum(z$duration_ms, na.rm = TRUE) / trial_duration,
      first_fixation_latency_ms = (min(z$start_time, na.rm = TRUE) - tr$start_time[1L]) * 1000,
      revisits = revisits,
      gaze_entropy = entropy
    )
    base <- list(
      recording_id = rec, participant_id = tr$participant_id[1L], trial_id = trial,
      item_id = tr$item_id[1L], stimulus_id = tr$stimulus_id[1L],
      aoi_id = if (level == "trial_aoi") z$aoi_id[1L] else NA_character_
    )
    units <- c("count", "milliseconds", "milliseconds", "milliseconds", "milliseconds", "proportion", "milliseconds", "count", "bits")
    k <- k + 1L
    rows[[k]] <- .feature_rows(base, vals, units, level, paste0("derive_gaze_features:", source), observed_fraction = mean(is.finite(z$start_time)))
  }
  f <- if (length(rows)) do.call(.bind_rows_base, rows) else empty_eye_table("features")
  if (append) x$features <- standardize_eye_table(.bind_rows_base(x$features, f), "features") else x$features <- standardize_eye_table(f, "features")
  add_provenance(x, "derive_gaze_features", "features", paste0(nrow(f), " feature rows; level=", level, ";source=", source))
}

derive_pupil_features <- function(x, level = c("trial", "trial_aoi"), append = TRUE, pupil_column = "pupil_diameter") {
  .assert_eye_dataset(x)
  level <- match.arg(level)
  d <- x$eye_samples
  if (!nrow(d)) return(x)
  if (!pupil_column %in% names(d)) .eye_stop("Pupil column `", pupil_column, "` is absent.")
  trials <- trial_table(x)
  if (!nrow(trials)) .eye_stop("Trial intervals are required.")
  if (level == "trial_aoi") .eye_warn("Pupil samples are not directly AOI-labelled; trial-level features will be returned unless AOI labels exist in `eye_samples`.")
  keys <- c("recording_id", "trial_id", "eye")
  groups <- .group_split(d[!is.na(d$trial_id), ], keys)
  rows <- list(); k <- 0L
  for (z in groups) {
    tr <- trials[trials$recording_id == z$recording_id[1L] & trials$trial_id == z$trial_id[1L], ]
    if (!nrow(tr)) next
    y <- z[[pupil_column]]; t <- z$timestamp_seconds
    ok <- is.finite(y) & is.finite(t)
    if (!any(ok)) next
    ord <- order(t[ok]); yy <- y[ok][ord]; tt <- t[ok][ord]
    fit <- if (length(yy) >= 2L) stats::lm(yy ~ tt) else NULL
    peak_idx <- which.max(yy)
    vals <- c(
      pupil_mean = mean(yy), pupil_sd = stats::sd(yy), pupil_peak = max(yy),
      pupil_minimum = min(yy), pupil_auc = .trapz(tt - min(tt), yy),
      pupil_slope = if (is.null(fit)) NA_real_ else unname(stats::coef(fit)[2L]),
      pupil_latency_peak_ms = (tt[peak_idx] - tr$start_time[1L]) * 1000,
      pupil_observed_fraction = mean(is.finite(y)),
      pupil_interpolated_fraction = if ("interpolated" %in% names(z)) mean(z$interpolated, na.rm = TRUE) else 0
    )
    base <- list(
      recording_id = z$recording_id[1L], participant_id = tr$participant_id[1L], trial_id = z$trial_id[1L],
      item_id = tr$item_id[1L], stimulus_id = tr$stimulus_id[1L], aoi_id = NA_character_
    )
    units <- c(z$pupil_unit[which(ok)[1L]], z$pupil_unit[which(ok)[1L]], z$pupil_unit[which(ok)[1L]], z$pupil_unit[which(ok)[1L]],
      paste0(z$pupil_unit[which(ok)[1L]], "*seconds"), paste0(z$pupil_unit[which(ok)[1L]], "/second"), "milliseconds", "proportion", "proportion")
    k <- k + 1L
    rows[[k]] <- .feature_rows(base, vals, units, "trial_eye", paste0("derive_pupil_features:", pupil_column),
      observed_fraction = mean(is.finite(y)), parameters = paste0("eye=", z$eye[1L]))
  }
  f <- if (length(rows)) do.call(.bind_rows_base, rows) else empty_eye_table("features")
  if (append) x$features <- standardize_eye_table(.bind_rows_base(x$features, f), "features") else x$features <- standardize_eye_table(f, "features")
  add_provenance(x, "derive_pupil_features", "features", paste0(nrow(f), " feature rows"))
}

derive_rt_features <- function(x, append = TRUE) {
  .assert_eye_dataset(x)
  r <- x$responses
  if (!nrow(r)) return(x)
  rows <- lapply(seq_len(nrow(r)), function(i) {
    vals <- c(response_time = r$response_time[i], score = r$score[i])
    base <- as.list(r[i, c("recording_id", "participant_id", "trial_id", "item_id"), drop = FALSE])
    base$stimulus_id <- NA_character_; base$aoi_id <- NA_character_
    .feature_rows(base, vals, c("seconds", "score"), "response", "derive_rt_features")
  })
  f <- do.call(.bind_rows_base, rows)
  if (append) x$features <- standardize_eye_table(.bind_rows_base(x$features, f), "features") else x$features <- standardize_eye_table(f, "features")
  add_provenance(x, "derive_rt_features", "features", paste0(nrow(f), " feature rows"))
}

derive_biometric_features <- function(x, append = TRUE) {
  .assert_eye_dataset(x)
  d <- x$biometrics
  if (!nrow(d)) return(x)
  trials <- trial_table(x)
  groups <- .group_split(d[!is.na(d$trial_id), ], c("recording_id", "trial_id", "channel"))
  rows <- list(); k <- 0L
  for (z in groups) {
    y <- z$value; t <- z$timestamp_seconds; ok <- is.finite(y) & is.finite(t)
    if (!any(ok)) next
    tr <- trials[trials$recording_id == z$recording_id[1L] & trials$trial_id == z$trial_id[1L], ]
    vals <- c(
      biometric_mean = mean(y[ok]), biometric_sd = stats::sd(y[ok]),
      biometric_min = min(y[ok]), biometric_max = max(y[ok]),
      biometric_auc = .trapz(t[ok] - min(t[ok]), y[ok]),
      biometric_observed_fraction = mean(ok)
    )
    names(vals) <- paste0(z$channel[1L], "_", sub("biometric_", "", names(vals)))
    base <- list(
      recording_id = z$recording_id[1L], participant_id = if (nrow(tr)) tr$participant_id[1L] else NA_character_,
      trial_id = z$trial_id[1L], item_id = if (nrow(tr)) tr$item_id[1L] else NA_character_,
      stimulus_id = if (nrow(tr)) tr$stimulus_id[1L] else z$stimulus_id[1L], aoi_id = NA_character_
    )
    unit <- .first_nonmissing(z$unit, "unknown")
    units <- c(unit, unit, unit, unit, paste0(unit, "*seconds"), "proportion")
    k <- k + 1L
    rows[[k]] <- .feature_rows(base, vals, units, "trial_channel", "derive_biometric_features", observed_fraction = mean(ok), parameters = paste0("channel=", z$channel[1L]))
  }
  f <- if (length(rows)) do.call(.bind_rows_base, rows) else empty_eye_table("features")
  if (append) x$features <- standardize_eye_table(.bind_rows_base(x$features, f), "features") else x$features <- standardize_eye_table(f, "features")
  add_provenance(x, "derive_biometric_features", "features", paste0(nrow(f), " feature rows"))
}

derive_all_features <- function(x, spec = feature_spec(), reset = FALSE) {
  .assert_eye_dataset(x)
  if (!inherits(spec, "eye_feature_spec")) .eye_stop("`spec` must be created with `feature_spec()`.")
  if (reset) x$features <- empty_eye_table("features")
  if (nrow(x$episodes) || nrow(x$gaze_samples)) {
    source <- if (any(x$episodes$episode_type == "fixation")) "fixations" else "samples"
    x <- derive_gaze_features(x, level = if (spec$level == "trial_aoi") "trial_aoi" else "trial", source = source)
  }
  if (nrow(x$eye_samples)) x <- derive_pupil_features(x)
  if (spec$response_time && nrow(x$responses)) x <- derive_rt_features(x)
  if (spec$biometrics && nrow(x$biometrics)) x <- derive_biometric_features(x)
  add_provenance(x, "derive_all_features", "features", paste0("total_rows=", nrow(x$features)))
}

features_wide <- function(x, id_cols = c("recording_id", "participant_id", "trial_id", "item_id", "stimulus_id", "aoi_id"), aggregate = mean) {
  .assert_eye_dataset(x)
  d <- x$features
  if (!nrow(d)) return(data.frame())
  id_cols <- intersect(id_cols, names(d))
  key <- interaction(d[id_cols], drop = TRUE, lex.order = TRUE)
  groups <- split(d, key)
  rows <- lapply(groups, function(z) {
    base <- z[1L, id_cols, drop = FALSE]
    vals <- tapply(z$value, z$feature_name, aggregate, na.rm = TRUE)
    cbind(base, as.data.frame(as.list(vals), stringsAsFactors = FALSE))
  })
  out <- do.call(rbind, rows); rownames(out) <- NULL; out
}

feature_dictionary <- function(x) {
  .assert_eye_dataset(x)
  d <- x$features
  if (!nrow(d)) return(data.frame())
  unique(d[c("feature_name", "unit", "level", "method", "parameters")])
}
