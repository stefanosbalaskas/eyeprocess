# Gazepoint Analysis 7.x empirical export support -------------------------

.gp_id_token <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | !nzchar(x)] <- "unknown"
  gsub("_+", "_", gsub("[^A-Za-z0-9]+", "_", x))
}

.gp_filename_identity <- function(
    path,
    participant_id = NULL,
    recording_id = NULL,
    session_id = "S001") {
  base <- tools::file_path_sans_ext(basename(path))
  stem <- sub("(?i)(?:[_ -](?:all[_ -]?gaze|fixations?|user(?:[_ -]?fix)?))$", "", base, perl = TRUE)
  user_match <- regexec("(?i)^user[ _-]*([0-9]+)$", stem, perl = TRUE)
  hit <- regmatches(stem, user_match)[[1L]]
  inferred <- if (length(hit) == 2L) paste("User", hit[2L]) else trimws(stem)
  if (!nzchar(inferred) || grepl("(?i)^data[_ -]?summary", inferred, perl = TRUE)) inferred <- "P001"
  participant <- participant_id %||% inferred
  recording <- recording_id %||% paste0("rec_", .gp_id_token(participant), "_", .gp_id_token(session_id))
  list(
    participant_id = as.character(participant),
    recording_id = as.character(recording),
    session_id = as.character(session_id),
    source_stem = stem
  )
}

.gp_time_info <- function(data) {
  nms <- names(data)
  media_col <- grep("^TIME(?:\\(|$)", nms, value = TRUE, ignore.case = TRUE)[1L]
  tick_col <- grep("^TIMETICK(?:\\(|$)", nms, value = TRUE, ignore.case = TRUE)[1L]
  if (is.na(media_col)) media_col <- NULL
  if (is.na(tick_col)) tick_col <- NULL
  frequency <- NA_real_
  if (!is.null(tick_col)) {
    m <- regexec("(?i)f\\s*=\\s*([0-9.]+)", tick_col, perl = TRUE)
    z <- regmatches(tick_col, m)[[1L]]
    if (length(z) == 2L) frequency <- suppressWarnings(as.numeric(z[2L]))
  }
  recording_start <- NA_character_
  if (!is.null(media_col)) {
    m <- regexec("^TIME\\((.*)\\)$", media_col, perl = TRUE)
    z <- regmatches(media_col, m)[[1L]]
    if (length(z) == 2L) recording_start <- z[2L]
  }
  list(
    media_time_col = media_col,
    tick_col = tick_col,
    tick_frequency = frequency,
    recording_start = recording_start
  )
}

.gp_tick_origin <- function(data, info) {
  if (is.null(info$tick_col) || !is.finite(info$tick_frequency)) return(NA_real_)
  tick <- .safe_numeric(data[[info$tick_col]])
  media_time <- if (!is.null(info$media_time_col)) .safe_numeric(data[[info$media_time_col]]) else rep(NA_real_, length(tick))
  keep <- is.finite(tick)
  if (!any(keep)) return(NA_real_)
  first <- which(keep)[1L]
  if (is.finite(media_time[first])) tick[first] - media_time[first] * info$tick_frequency else tick[first]
}

.gp_source_lookup <- function(data, info) {
  if (is.null(info$tick_col)) return(NULL)
  tick <- .safe_numeric(data[[info$tick_col]])
  out <- data.frame(
    timestamp_native = tick,
    media_time_seconds = if (!is.null(info$media_time_col)) .safe_numeric(data[[info$media_time_col]]) else NA_real_,
    media_id_source = as.character(.map_column(data, list(media = .first_existing(names(data), c("MEDIA_ID"))), "media")),
    media_name_source = as.character(.map_column(data, list(media = .first_existing(names(data), c("MEDIA_NAME"))), "media")),
    sample_index_source = .safe_numeric(.map_column(data, list(cnt = .first_existing(names(data), c("CNT"))), "cnt")),
    aoi_label_source = as.character(.map_column(data, list(aoi = .first_existing(names(data), c("AOI"))), "aoi")),
    saccade_magnitude_source = .safe_numeric(.map_column(data, list(v = .first_existing(names(data), c("SACCADE_MAG"))), "v")),
    saccade_direction_source = .safe_numeric(.map_column(data, list(v = .first_existing(names(data), c("SACCADE_DIR"))), "v")),
    video_frame_source = .safe_numeric(.map_column(data, list(v = .first_existing(names(data), c("VID_FRAME"))), "v")),
    stringsAsFactors = FALSE
  )
  out[!duplicated(out$timestamp_native), , drop = FALSE]
}

.gp_apply_timebase <- function(out, data, info, origin_tick = NULL) {
  if (is.null(info$tick_col) || !is.finite(info$tick_frequency)) return(out)
  origin_tick <- origin_tick %||% .gp_tick_origin(data, info)
  if (!is.finite(origin_tick)) return(out)
  normalize <- function(x) (.safe_numeric(x) - origin_tick) / info$tick_frequency
  lookup <- .gp_source_lookup(data, info)
  targets <- intersect(c("gaze_samples", "eye_samples", "biometrics", "events"), names(out))
  for (table in targets) {
    d <- out[[table]]
    if (!nrow(d) || !"timestamp_native" %in% names(d)) next
    d$timestamp_seconds <- normalize(d$timestamp_native)
    if (!is.null(lookup)) {
      idx <- match(.safe_numeric(d$timestamp_native), lookup$timestamp_native)
      for (field in setdiff(names(lookup), "timestamp_native")) d[[field]] <- lookup[[field]][idx]
    }
    out[[table]] <- d
  }
  if (nrow(out$recordings)) {
    out$recordings$recording_start <- info$recording_start
    out$recordings$nominal_sampling_rate[!is.finite(out$recordings$nominal_sampling_rate)] <- 60
  }
  if (nrow(out$streams)) {
    out$streams$timestamp_unit <- "ticks"
    out$streams$source_clock <- "Gazepoint TIMETICK"
    gaze_streams <- out$streams$stream_type == "gaze_combined"
    if (any(gaze_streams) && nrow(out$gaze_samples)) {
      rates <- vapply(out$streams$recording_id[gaze_streams], function(id) {
        estimate_sampling_rate(out$gaze_samples$timestamp_seconds[out$gaze_samples$recording_id == id])
      }, numeric(1))
      out$streams$observed_rate_hz[gaze_streams] <- rates
    }
  }
  out$vendor_metadata$gazepoint_timebase <- list(
    native_clock = info$tick_col,
    tick_frequency = info$tick_frequency,
    origin_tick = origin_tick,
    recording_start = info$recording_start,
    media_relative_clock = info$media_time_col,
    normalized_clock = "seconds since recording start"
  )
  out
}

.gp_aoi_id <- function(media_id, aoi) {
  media <- .gp_id_token(media_id)
  label <- trimws(as.character(aoi))
  missing <- is.na(label) | !nzchar(label)
  numeric_id <- sub("(?i)^AOI\\s*", "", label, perl = TRUE)
  numeric_id <- .gp_id_token(numeric_id)
  out <- paste0("media_", media, "_aoi_", numeric_id)
  out[missing] <- NA_character_
  out
}

.gp_biometric_mapping <- function(data) {
  nms <- names(data)
  candidates <- list(
    gsr_raw = c("GSR", "GSR_RAW"),
    eda = c("GSR_US", "EDA", "SKIN_CONDUCTANCE"),
    skin_conductance_level = c("GSR_US_TONIC", "GSR_SCL", "SCL"),
    skin_conductance_response = c("GSR_US_PHASIC", "GSR_SCR", "SCR"),
    heart_rate = c("HR", "HEART_RATE", "HEARTRATE", "Heart Rate"),
    heart_rate_period = c("HRP"),
    interbeat_interval = c("IBI", "INTERBEAT_INTERVAL", "RR_INTERVAL"),
    engagement_dial = c("DIAL", "ENGAGEMENT", "ENGAGEMENT_DIAL")
  )
  out <- lapply(candidates, function(x) .first_existing(nms, x))
  out <- out[!vapply(out, is.null, logical(1))]
  unlist(out, use.names = TRUE)
}

.gp_mapping <- function(data) {
  nms <- names(data)
  info <- .gp_time_info(data)
  timestamp <- info$tick_col %||% info$media_time_col %||% .gp_pick(nms, "time")
  mapping <- eye_mapping(
    participant = .gp_pick(nms, "participant"),
    recording = .gp_pick(nms, "recording"),
    timestamp = timestamp,
    x = .gp_pick(nms, "x"),
    y = .gp_pick(nms, "y"),
    left_x = .gp_pick(nms, "left_x"),
    left_y = .gp_pick(nms, "left_y"),
    right_x = .gp_pick(nms, "right_x"),
    right_y = .gp_pick(nms, "right_y"),
    gaze_valid = .gp_pick(nms, "valid"),
    left_valid = .gp_pick(nms, "left_valid"),
    right_valid = .gp_pick(nms, "right_valid"),
    pupil_left = .gp_pick(nms, "pupil_left"),
    pupil_right = .gp_pick(nms, "pupil_right"),
    pupil_left_valid = .gp_pick(nms, "pupil_left_valid"),
    pupil_right_valid = .gp_pick(nms, "pupil_right_valid"),
    fixation_id = .gp_pick(nms, "fixation_id"),
    blink_id = .first_existing(nms, c("BKID", "BLINK_ID")),
    trial = .gp_pick(nms, "trial"),
    stimulus = .gp_pick(nms, "stimulus"),
    event_name = .gp_pick(nms, "marker"),
    biometric_channels = .gp_biometric_mapping(data)
  )
  mapping <- mapping[!vapply(mapping, is.null, logical(1))]
  class(mapping) <- "eye_mapping"
  mapping
}

.gp_is_summary_report <- function(path) {
  if (dir.exists(path) || !file.exists(path)) return(FALSE)
  if (grepl("(?i)^Data[_ -]?Summary[_ -]?export.*\\.csv$", basename(path), perl = TRUE)) return(TRUE)
  lines <- tryCatch(readLines(path, n = 20L, warn = FALSE), error = function(e) character())
  any(trimws(lines) == "AOI Summary") && any(grepl("^Gazepoint Analysis", lines))
}

is_gazepoint_export <- function(path, inspect_rows = 20L) {
  if (dir.exists(path)) {
    files <- list.files(path, pattern = "\\.(csv|txt|tsv)$", full.names = TRUE, ignore.case = TRUE)
    if (!length(files)) return(0)
    return(max(vapply(files, is_gazepoint_export, numeric(1), inspect_rows = inspect_rows)))
  }
  ext <- tolower(tools::file_ext(path))
  if (!ext %in% c("csv", "txt", "tsv")) return(0)
  if (.gp_is_summary_report(path)) return(0.99)
  base <- basename(path)
  filename_signature <- grepl("(?i)(?:_all[_ -]?gaze|_fixations?|[-_]user(?:[-_]fix)?)\\.csv$", base, perl = TRUE)
  header <- tryCatch(names(.read_delimited(path, nrows = min(2L, inspect_rows))), error = function(e) character())
  if (!length(header)) return(if (filename_signature) 0.75 else 0)
  gp_hits <- sum(toupper(header) %in% unique(toupper(unlist(.gp_columns, use.names = FALSE))))
  signature <- any(c("FPOGX", "BPOGX", "LPOGX", "RPOGX", "FPOGV", "BPOGV", "TIMETICK(F=10000000)") %in% toupper(header))
  score <- min(1, gp_hits / 8)
  if (signature) score <- max(score, 0.85)
  if (filename_signature || grepl("CurrentAOIStatistics", base, ignore.case = TRUE)) score <- max(score, 0.95)
  score
}

gp_identify_export_type <- function(path) {
  if (dir.exists(path)) return("folder")
  base <- basename(path)
  if (.gp_is_summary_report(path) || grepl("CurrentAOIStatistics", base, ignore.case = TRUE)) return("aoi_statistics")
  if (grepl("(?i)(?:_fixations?|[-_]user[-_]?fix)\\.csv$", base, perl = TRUE)) return("fixations")
  d <- tryCatch(.read_delimited(path, nrows = 3L), error = function(e) NULL)
  nms <- if (is.null(d)) character() else toupper(names(d))
  if (all(c("FPOGS", "FPOGD") %in% nms) && nrow(d) && anyDuplicated(d$FPOGID %||% integer()) == 0L &&
      !grepl("(?i)_all[_ -]?gaze", base, perl = TRUE)) return("fixations")
  has_gaze <- any(c("BPOGX", "FPOGX", "LPOGX", "RPOGX") %in% nms)
  has_biometrics <- any(grepl("^(HR|GSR|EDA|DIAL|IBI)", nms))
  if (has_gaze && has_biometrics) return("combined_biometrics")
  if (has_gaze) return("gaze")
  "unknown"
}

.gp_apply_biometric_validity <- function(out, data) {
  if (!nrow(out$biometrics)) return(out)
  info <- .gp_time_info(data)
  if (is.null(info$tick_col)) return(out)
  tick <- .safe_numeric(data[[info$tick_col]])
  validity_columns <- c(
    gsr_raw = "GSRV", eda = "GSRV", skin_conductance_level = "GSRV",
    skin_conductance_response = "GSRV", heart_rate = "HRV",
    heart_rate_period = "HRV", interbeat_interval = "HRV", engagement_dial = "DIALV"
  )
  units <- c(
    gsr_raw = "vendor_raw", eda = "microsiemens",
    skin_conductance_level = "microsiemens", skin_conductance_response = "microsiemens",
    heart_rate = "beats_per_minute", heart_rate_period = "vendor_units",
    interbeat_interval = "seconds", engagement_dial = "proportion"
  )
  for (channel in unique(out$biometrics$channel)) {
    idx <- out$biometrics$channel == channel
    source_valid <- validity_columns[[channel]]
    if (!is.null(source_valid) && source_valid %in% names(data)) {
      m <- match(.safe_numeric(out$biometrics$timestamp_native[idx]), tick)
      out$biometrics$valid[idx] <- .safe_logical(data[[source_valid]][m])
    }
    if (channel %in% names(units)) out$biometrics$unit[idx] <- units[[channel]]
  }
  if (nrow(out$streams)) {
    m <- match(out$streams$stream_type, names(units))
    keep <- !is.na(m)
    out$streams$value_unit[keep] <- unname(units[m[keep]])
  }
  out
}

read_gazepoint <- function(
    path,
    participant_id = NULL,
    recording_id = NULL,
    session_id = "S001",
    nominal_sampling_rate = 60,
    screen_width = NA_real_,
    screen_height = NA_real_,
    keep_raw = TRUE,
    quiet = FALSE,
    ...) {
  if (dir.exists(path)) return(read_gazepoint_folder(path, participant_id = participant_id, recording_id = recording_id, session_id = session_id, keep_raw = keep_raw, quiet = quiet, ...))
  type <- gp_identify_export_type(path)
  if (type == "fixations") return(read_gazepoint_fixations(path, participant_id = participant_id, recording_id = recording_id, session_id = session_id, keep_raw = keep_raw, quiet = quiet, ...))
  if (type == "aoi_statistics") return(read_gazepoint_aoi_statistics(path, participant_id = participant_id, recording_id = recording_id, session_id = session_id, keep_raw = keep_raw, quiet = quiet, ...))
  data <- .read_delimited(path, ...)
  identity <- .gp_filename_identity(path, participant_id, recording_id, session_id)
  info <- .gp_time_info(data)
  mapping <- .gp_mapping(data)
  if (!all(c("timestamp", "x", "y") %in% names(mapping))) {
    .eye_stop("Gazepoint sample export is missing identifiable time and gaze-coordinate columns.")
  }
  pupil_unit <- if (any(c("LPMM", "RPMM", "LPMMV", "RPMMV") %in% names(data))) "millimetres" else "vendor_units"
  out <- read_eye_generic(
    data,
    mapping = mapping,
    time_unit = if (!is.null(info$tick_col)) "ticks" else "seconds",
    coordinate_space = "display_normalized_top_left",
    screen_width = screen_width,
    screen_height = screen_height,
    pupil_unit = pupil_unit,
    vendor = "Gazepoint",
    recording_id = identity$recording_id,
    participant_id = identity$participant_id,
    session_id = identity$session_id,
    nominal_sampling_rate = nominal_sampling_rate,
    keep_raw = keep_raw,
    quiet = TRUE
  )
  out <- .gp_apply_timebase(out, data, info)
  out <- .gp_apply_biometric_validity(out, data)
  out$recordings$device_model <- "Gazepoint"
  out$recordings$software_name <- "Gazepoint Analysis"
  out$vendor_metadata$gazepoint <- list(
    export_type = type,
    source_columns = names(data),
    source_file = normalizePath(path, winslash = "/", mustWork = FALSE),
    source_identity = identity,
    coordinate_convention = "normalized top-left; out-of-range values retained",
    biometric_channels = .gp_biometric_mapping(data)
  )
  if (isTRUE(keep_raw)) out$raw$gazepoint <- data
  out <- gp_parse_user_events(out)
  out <- gp_parse_media_events(out)
  out <- add_provenance(out, "import_gazepoint_v7", "dataset", paste0("Gazepoint export type: ", type), source_files = path)
  attr(out, "validation") <- validate_eye_dataset(out)
  .eye_message("Imported Gazepoint export: ", basename(path), quiet = quiet)
  out
}

read_gazepoint_fixations <- function(
    path,
    participant_id = NULL,
    recording_id = NULL,
    session_id = "S001",
    origin_tick = NULL,
    keep_raw = TRUE,
    quiet = FALSE,
    ...) {
  data <- .read_delimited(path, ...)
  nms <- names(data)
  identity <- .gp_filename_identity(path, participant_id, recording_id, session_id)
  info <- .gp_time_info(data)
  pick <- function(...) .first_existing(nms, c(...))
  duration_seconds <- .safe_numeric(.map_column(data, list(duration = pick("FPOGD", "FIXATION_DURATION", "duration")), "duration"))
  duration_ms <- if (all(duration_seconds < 20, na.rm = TRUE)) duration_seconds * 1000 else duration_seconds
  tick <- if (!is.null(info$tick_col)) .safe_numeric(data[[info$tick_col]]) else rep(NA_real_, nrow(data))
  if (is.null(origin_tick) || !is.finite(origin_tick)) origin_tick <- .gp_tick_origin(data, info)
  if (is.finite(origin_tick) && is.finite(info$tick_frequency)) {
    end <- (tick - origin_tick) / info$tick_frequency
    start <- end - duration_ms / 1000
  } else {
    start <- .safe_numeric(.map_column(data, list(start = pick("FPOGS", "FIXATION_START", "start_time")), "start"))
    end <- start + duration_ms / 1000
  }
  fixation_id <- as.character(.map_column(data, list(fixation_id = pick("FPOGID", "FIXATION_ID", "fixation_id")), "fixation_id"))
  stimulus <- .as_character_id(.map_column(data, list(stimulus = pick("MEDIA_ID", "MEDIA_NAME", "stimulus_id")), "stimulus"))
  media_name <- as.character(.map_column(data, list(stimulus = pick("MEDIA_NAME")), "stimulus"))
  raw_aoi <- as.character(.map_column(data, list(aoi = pick("AOI", "AOI_ID", "aoi_id")), "aoi"))
  missing_id <- is.na(fixation_id) | !nzchar(fixation_id)
  fixation_id[missing_id] <- sprintf("%07d", which(missing_id))
  episode_id <- paste0(identity$recording_id, "_fix_", .gp_id_token(stimulus), "_", .gp_id_token(fixation_id))
  if (anyDuplicated(episode_id)) episode_id <- paste0(episode_id, "_row", sprintf("%05d", seq_along(episode_id)))
  episodes <- data.frame(
    episode_id = episode_id,
    recording_id = identity$recording_id,
    episode_type = "fixation",
    eye = "combined",
    start_time = start,
    end_time = end,
    duration_ms = duration_ms,
    start_x = NA_real_, start_y = NA_real_, end_x = NA_real_, end_y = NA_real_,
    centroid_x = .safe_numeric(.map_column(data, list(x = pick("FPOGX", "FIXATION_X", "x")), "x")),
    centroid_y = .safe_numeric(.map_column(data, list(y = pick("FPOGY", "FIXATION_Y", "y")), "y")),
    amplitude = NA_real_, peak_velocity = NA_real_, dispersion = NA_real_,
    coordinate_space_id = "coord_display_normalized_top_left",
    source_algorithm = "Gazepoint Analysis",
    source_parameters = NA_character_,
    derived_by = "vendor",
    trial_id = NA_character_,
    stimulus_id = stimulus,
    aoi_id = .gp_aoi_id(stimulus, raw_aoi),
    timestamp_native = tick,
    source_fixation_id = fixation_id,
    source_media_time_start = .safe_numeric(.map_column(data, list(start = pick("FPOGS")), "start")),
    source_media_time_end = if (!is.null(info$media_time_col)) .safe_numeric(data[[info$media_time_col]]) else NA_real_,
    source_media_name = media_name,
    source_aoi_label = raw_aoi,
    stringsAsFactors = FALSE
  )
  recordings <- data.frame(
    recording_id = identity$recording_id,
    participant_id = identity$participant_id,
    session_id = identity$session_id,
    vendor = "Gazepoint", vendor_family = "Gazepoint", device_model = "Gazepoint",
    firmware_version = NA_character_, software_name = "Gazepoint Analysis",
    software_version = NA_character_, experiment_type = NA_character_,
    nominal_sampling_rate = 60, screen_width_px = NA_real_, screen_height_px = NA_real_,
    recording_start = info$recording_start, source_timezone = NA_character_,
    source_file_set = normalizePath(path, winslash = "/", mustWork = FALSE),
    stringsAsFactors = FALSE
  )
  coord <- new_coordinate_space("coord_display_normalized_top_left", "display_normalized_top_left")
  out <- new_eye_dataset(
    recordings = recordings, episodes = episodes, coordinate_spaces = coord,
    raw = if (keep_raw) list(gazepoint_fixations = data) else list(),
    vendor_metadata = list(gazepoint_fixations = list(
      source_columns = names(data), source_identity = identity,
      timebase = list(tick_col = info$tick_col, tick_frequency = info$tick_frequency, origin_tick = origin_tick)
    )),
    validate = FALSE
  )
  out <- add_provenance(out, "import_gazepoint_fixations_v7", "episodes", paste0(nrow(episodes), " fixations"), source_files = path)
  attr(out, "validation") <- validate_eye_dataset(out)
  .eye_message("Imported ", nrow(episodes), " Gazepoint fixations.", quiet = quiet)
  out
}

.gp_parse_csv_block <- function(lines, title) {
  start <- which(trimws(lines) == title)[1L]
  if (is.na(start) || start >= length(lines)) return(data.frame())
  header <- start + 1L
  end <- header + 1L
  while (end <= length(lines) && nzchar(trimws(lines[end]))) end <- end + 1L
  rows <- lines[seq.int(header, max(header, end - 1L))]
  rows <- sub(",\\s*$", "", rows)
  text <- paste(rows, collapse = "\n")
  out <- tryCatch(utils::read.csv(text = text, stringsAsFactors = FALSE, check.names = FALSE, strip.white = TRUE), error = function(e) data.frame())
  if (ncol(out)) {
    names(out) <- trimws(names(out))
    blank <- !nzchar(names(out))
    if (any(blank)) out <- out[!blank]
    out[] <- lapply(out, function(z) if (is.character(z)) trimws(z) else z)
  }
  out
}

read_gazepoint_summary <- function(path) {
  if (!.gp_is_summary_report(path)) .eye_stop("Not a recognized Gazepoint Data Summary export: ", path)
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  first <- strsplit(lines[1L], ",", fixed = TRUE)[[1L]]
  version <- if (length(first) >= 2L) trimws(first[2L]) else NA_character_
  processed <- if (length(lines) >= 2L) {
    z <- strsplit(lines[2L], ",", fixed = TRUE)[[1L]]
    if (length(z) >= 2L) trimws(paste(z[-1L], collapse = ",")) else NA_character_
  } else NA_character_
  out <- list(
    path = normalizePath(path, winslash = "/", mustWork = FALSE),
    software = trimws(first[1L]),
    software_version = version,
    processed_on = processed,
    aoi_summary = .gp_parse_csv_block(lines, "AOI Summary"),
    aoi_statistics = .gp_parse_csv_block(lines, "AOI Statistics (for each user)"),
    notes = lines[grepl("^Note:", trimws(lines))]
  )
  class(out) <- "gazepoint_summary"
  out
}

print.gazepoint_summary <- function(x, ...) {
  cat("<gazepoint_summary>\n")
  cat("  Software:       ", x$software, " ", x$software_version, "\n", sep = "")
  cat("  Processed:      ", x$processed_on, "\n", sep = "")
  cat("  AOI summaries:  ", nrow(x$aoi_summary), "\n", sep = "")
  cat("  User-AOI rows:  ", nrow(x$aoi_statistics), "\n", sep = "")
  invisible(x)
}

.gp_summary_features <- function(summary, session_id = "S001") {
  d <- summary$aoi_statistics
  if (!nrow(d)) return(empty_eye_table("features"))
  pick <- function(...) .first_existing(names(d), c(...))
  media <- as.character(d[[pick("Media ID")]])
  aoi_source <- as.character(d[[pick("AOI ID", "AOI Name")]])
  aoi_id <- .gp_aoi_id(media, aoi_source)
  participant <- as.character(d[[pick("User Name", "User ID")]])
  participant[is.na(participant) | !nzchar(participant)] <- paste0("User ", d[[pick("User ID")]])
  recording <- paste0("rec_", .gp_id_token(participant), "_", .gp_id_token(session_id))
  window_start <- .safe_numeric(d[[pick("AOI Start", "AOI Start (sec)")]])
  duration <- .safe_numeric(d[[pick("AOI Duration (sec - U=UserControlled)")]])
  window_end <- window_start + duration
  metrics <- list(
    time_to_first_view = list(c("Time to 1st View (sec) -1.0 means not viewed"), "seconds"),
    time_viewed = list(c("Time Viewed (sec)"), "seconds"),
    time_viewed_percent = list(c("Time Viewed (%)"), "percent"),
    fixation_count = list(c("Fixations (#)"), "count"),
    revisit_count = list(c("Revisits (#)"), "count"),
    click_count = list(c("Clicks (#)"), "count"),
    mean_dial = list(c("Ave Dial (0-1)"), "proportion"),
    mean_gsr_vendor_reported = list(c("Ave GSR (kOhm)"), "vendor_reported_kOhm"),
    mean_heart_rate = list(c("Ave Heart Rate (BPM)"), "beats_per_minute"),
    mean_interbeat_interval = list(c("Ave Interbeat Interval (s)"), "seconds"),
    mean_left_pupil = list(c("Ave Left Pupil (mm)"), "millimetres"),
    mean_right_pupil = list(c("Ave Right Pupil (mm)"), "millimetres")
  )
  rows <- list()
  k <- 0L
  for (feature_name in names(metrics)) {
    col <- .first_existing(names(d), metrics[[feature_name]][[1L]])
    if (is.null(col)) next
    value <- .safe_numeric(d[[col]])
    if (feature_name == "time_to_first_view") value[value < 0] <- NA_real_
    k <- k + 1L
    feature_id <- paste0(
      "feature_", .gp_id_token(participant), "_", .gp_id_token(media), "_",
      .gp_id_token(aoi_source), "_", .gp_id_token(feature_name), "_",
      .gp_id_token(summary$processed_on)
    )
    if (anyDuplicated(feature_id)) feature_id <- paste0(feature_id, "_row", sprintf("%05d", seq_along(feature_id)))
    rows[[k]] <- data.frame(
      feature_id = feature_id,
      recording_id = recording,
      participant_id = participant,
      trial_id = NA_character_, item_id = NA_character_,
      stimulus_id = media, aoi_id = aoi_id,
      feature_name = feature_name, value = value, unit = metrics[[feature_name]][[2L]],
      level = "participant_aoi_summary", window_start = window_start, window_end = window_end,
      observed_fraction = NA_real_, method = paste0("Gazepoint Analysis ", summary$software_version, " Data Summary"),
      parameters = paste0("processed_on=", summary$processed_on),
      derived_at = summary$processed_on,
      stringsAsFactors = FALSE
    )
  }
  viewed <- .safe_numeric(d[[pick("Time to 1st View (sec) -1.0 means not viewed")]]) >= 0
  k <- k + 1L
  viewed_id <- paste0(
    "feature_", .gp_id_token(participant), "_", .gp_id_token(media), "_",
    .gp_id_token(aoi_source), "_aoi_viewed_", .gp_id_token(summary$processed_on)
  )
  if (anyDuplicated(viewed_id)) viewed_id <- paste0(viewed_id, "_row", sprintf("%05d", seq_along(viewed_id)))
  rows[[k]] <- data.frame(
    feature_id = viewed_id, recording_id = recording,
    participant_id = participant, trial_id = NA_character_, item_id = NA_character_,
    stimulus_id = media, aoi_id = aoi_id, feature_name = "aoi_viewed",
    value = as.numeric(viewed), unit = "binary", level = "participant_aoi_summary",
    window_start = window_start, window_end = window_end, observed_fraction = NA_real_,
    method = paste0("Gazepoint Analysis ", summary$software_version, " Data Summary"),
    parameters = paste0("processed_on=", summary$processed_on), derived_at = summary$processed_on,
    stringsAsFactors = FALSE
  )
  do.call(.bind_rows_base, rows)
}

read_gazepoint_aoi_statistics <- function(
    path,
    participant_id = NULL,
    recording_id = NULL,
    session_id = "S001",
    keep_raw = TRUE,
    quiet = FALSE,
    ...) {
  if (!.gp_is_summary_report(path)) {
    d <- .read_delimited(path, ...)
    identity <- .gp_filename_identity(path, participant_id, recording_id, session_id)
    aoi_col <- .first_existing(names(d), c("AOI_ID", "AOI", "AOI Name", "aoi_id"))
    if (is.null(aoi_col)) .eye_stop("Cannot identify an AOI identifier column.")
    name_col <- .first_existing(names(d), c("AOI_NAME", "AOI", "AOI Name", "aoi_name")) %||% aoi_col
    stimulus_col <- .first_existing(names(d), c("MEDIA_ID", "MEDIA_NAME", "stimulus_id"))
    stimulus <- if (is.null(stimulus_col)) rep(NA_character_, nrow(d)) else .as_character_id(d[[stimulus_col]])
    defs <- data.frame(
      aoi_id = .gp_aoi_id(stimulus, d[[aoi_col]]),
      aoi_name = as.character(d[[name_col]]),
      stimulus_id = stimulus,
      shape_type = "unknown",
      coordinate_space_id = "coord_display_normalized_top_left",
      parent_aoi_id = NA_character_,
      source = "Gazepoint AOI statistics",
      stringsAsFactors = FALSE
    )
    defs <- defs[!is.na(defs$aoi_id) & !duplicated(defs$aoi_id), , drop = FALSE]
    recordings <- data.frame(
      recording_id = identity$recording_id, participant_id = identity$participant_id,
      session_id = identity$session_id, vendor = "Gazepoint", vendor_family = "Gazepoint",
      device_model = "Gazepoint", firmware_version = NA_character_,
      software_name = "Gazepoint Analysis", software_version = NA_character_,
      experiment_type = NA_character_, nominal_sampling_rate = 60,
      screen_width_px = NA_real_, screen_height_px = NA_real_,
      recording_start = NA_character_, source_timezone = NA_character_,
      source_file_set = normalizePath(path, winslash = "/", mustWork = FALSE),
      stringsAsFactors = FALSE
    )
    out <- new_eye_dataset(
      recordings = recordings, aoi_definitions = defs,
      coordinate_spaces = new_coordinate_space("coord_display_normalized_top_left", "display_normalized_top_left"),
      raw = if (keep_raw) list(gazepoint_aoi_statistics = d) else list(),
      vendor_metadata = list(gazepoint_aoi_statistics = list(source_columns = names(d))),
      validate = FALSE
    )
    out <- add_provenance(out, "import_gazepoint_aoi_statistics", "aoi_definitions", paste0(nrow(defs), " AOIs"), source_files = path)
    attr(out, "validation") <- validate_eye_dataset(out)
    .eye_message("Imported Gazepoint AOI statistics.", quiet = quiet)
    return(out)
  }
  summary <- read_gazepoint_summary(path)
  d <- summary$aoi_statistics
  s <- summary$aoi_summary
  source <- if (nrow(d)) d else s
  pick <- function(data, ...) .first_existing(names(data), c(...))
  media <- as.character(source[[pick(source, "Media ID")]])
  aoi_source <- as.character(source[[pick(source, "AOI ID", "AOI Name")]])
  aoi_name <- as.character(source[[pick(source, "AOI Name", "AOI ID")]])
  defs <- unique(data.frame(
    aoi_id = .gp_aoi_id(media, aoi_source),
    aoi_name = aoi_name,
    stimulus_id = media,
    shape_type = "unknown",
    coordinate_space_id = "coord_display_normalized_top_left",
    parent_aoi_id = NA_character_,
    source = paste0("Gazepoint Analysis ", summary$software_version, " Data Summary"),
    stringsAsFactors = FALSE
  ))
  defs <- defs[!is.na(defs$aoi_id), , drop = FALSE]
  if (nrow(d)) {
    user_name <- as.character(d[[pick(d, "User Name", "User ID")]])
    user_name[is.na(user_name) | !nzchar(user_name)] <- paste0("User ", d[[pick(d, "User ID")]])
  } else {
    user_name <- participant_id %||% "P001"
  }
  user_name <- unique(user_name)
  recordings <- data.frame(
    recording_id = if (!is.null(recording_id) && length(user_name) == 1L) recording_id else paste0("rec_", .gp_id_token(user_name), "_", .gp_id_token(session_id)),
    participant_id = if (!is.null(participant_id) && length(user_name) == 1L) participant_id else user_name,
    session_id = session_id,
    vendor = "Gazepoint", vendor_family = "Gazepoint", device_model = "Gazepoint",
    firmware_version = NA_character_, software_name = "Gazepoint Analysis",
    software_version = summary$software_version, experiment_type = NA_character_,
    nominal_sampling_rate = 60, screen_width_px = NA_real_, screen_height_px = NA_real_,
    recording_start = NA_character_, source_timezone = NA_character_,
    source_file_set = normalizePath(path, winslash = "/", mustWork = FALSE),
    stringsAsFactors = FALSE
  )
  out <- new_eye_dataset(
    recordings = recordings,
    aoi_definitions = defs,
    features = .gp_summary_features(summary, session_id),
    coordinate_spaces = new_coordinate_space("coord_display_normalized_top_left", "display_normalized_top_left"),
    raw = if (keep_raw) list(gazepoint_data_summary = list(aoi_summary = s, aoi_statistics = d)) else list(),
    vendor_metadata = list(gazepoint_data_summary = summary),
    validate = FALSE
  )
  out <- add_provenance(out, "import_gazepoint_data_summary", "aoi_definitions|features", paste0(nrow(defs), " AOIs; ", nrow(out$features), " summary features"), source_files = path)
  attr(out, "validation") <- validate_eye_dataset(out)
  .eye_message("Imported Gazepoint Data Summary: ", basename(path), quiet = quiet)
  out
}

.gp_dedupe_summary_features <- function(x) {
  if (!nrow(x$features)) return(x)
  d <- x$features
  is_summary <- grepl("Gazepoint Analysis.*Data Summary", d$method)
  if (!any(is_summary)) return(x)
  ordinary <- d[!is_summary, , drop = FALSE]
  summary <- d[is_summary, , drop = FALSE]
  key_fields <- c("recording_id", "participant_id", "stimulus_id", "aoi_id", "feature_name", "window_start", "window_end")
  key <- do.call(paste, c(lapply(summary[key_fields], .stable_cell), sep = "\r"))
  ord <- order(summary$derived_at, seq_len(nrow(summary)), na.last = TRUE)
  summary <- summary[ord, , drop = FALSE]
  key <- key[ord]
  summary <- summary[!duplicated(key, fromLast = TRUE), , drop = FALSE]
  x$features <- standardize_eye_table(.bind_rows_base(ordinary, summary), "features")
  x
}

read_gazepoint_folder <- function(
    path,
    include = c("gaze", "fixations", "events", "biometrics", "aoi"),
    participant_id = NULL,
    recording_id = NULL,
    session_id = "S001",
    keep_raw = TRUE,
    recursive = FALSE,
    quiet = FALSE,
    ...) {
  if (!dir.exists(path)) .eye_stop("Directory does not exist: ", path)
  files <- list.files(path, pattern = "\\.(csv|txt|tsv)$", full.names = TRUE, recursive = recursive, ignore.case = TRUE)
  if (!length(files)) .eye_stop("No delimited Gazepoint exports found.")
  profile <- gp_pair_exports(path)
  objs <- list()
  user_profile <- profile[profile$export_type %in% c("gaze", "combined_biometrics", "fixations"), , drop = FALSE]
  recording_groups <- unique(user_profile$group)
  if (!is.null(recording_id) && length(recording_groups) > 1L) {
    .eye_stop("recording_id can only be supplied when a Gazepoint folder contains one recording group; omit it for multi-recording folders so identifiers can be derived from filenames.")
  }
  for (group in recording_groups) {
    d <- user_profile[user_profile$group == group, , drop = FALSE]
    gaze_rows <- d$export_type %in% c("gaze", "combined_biometrics")
    if (any(gaze_rows) && any(c("gaze", "biometrics") %in% include)) {
      gaze_file <- d$file[which(gaze_rows)[1L]]
      identity <- .gp_filename_identity(gaze_file, participant_id = participant_id, recording_id = recording_id, session_id = session_id)
      gaze <- read_gazepoint(
        gaze_file,
        participant_id = identity$participant_id,
        recording_id = identity$recording_id,
        session_id = identity$session_id,
        keep_raw = keep_raw,
        quiet = TRUE,
        ...
      )
      objs[[length(objs) + 1L]] <- gaze
      if ("fixations" %in% include && any(d$export_type == "fixations")) {
        origin <- gaze$vendor_metadata$gazepoint_timebase$origin_tick %||% NA_real_
        for (fix_file in d$file[d$export_type == "fixations"]) {
          objs[[length(objs) + 1L]] <- read_gazepoint_fixations(
            fix_file,
            participant_id = identity$participant_id,
            recording_id = identity$recording_id,
            session_id = identity$session_id,
            origin_tick = origin,
            keep_raw = keep_raw,
            quiet = TRUE,
            ...
          )
        }
      }
    } else if ("fixations" %in% include && any(d$export_type == "fixations")) {
      for (fix_file in d$file[d$export_type == "fixations"]) {
        objs[[length(objs) + 1L]] <- read_gazepoint_fixations(fix_file, participant_id = participant_id, recording_id = recording_id, session_id = session_id, keep_raw = keep_raw, quiet = TRUE, ...)
      }
    }
  }
  if ("aoi" %in% include) {
    summary_files <- profile$file[profile$export_type == "aoi_statistics"]
    for (summary_file in summary_files) {
      objs[[length(objs) + 1L]] <- read_gazepoint_aoi_statistics(summary_file, session_id = session_id, keep_raw = keep_raw, quiet = TRUE, ...)
    }
  }
  if (!length(objs)) .eye_stop("No requested Gazepoint export types were found.")
  out <- do.call(combine_eye_datasets, c(objs, list(resolve_ids = FALSE)))
  out <- .gp_dedupe_summary_features(out)
  versions <- unique(unlist(lapply(out$vendor_metadata, function(z) z$software_version %||% NA_character_), use.names = FALSE))
  versions <- versions[!is.na(versions) & nzchar(versions)]
  if (length(versions) && nrow(out$recordings)) out$recordings$software_version <- versions[length(versions)]
  out$vendor_metadata$gazepoint_folder <- profile
  out <- add_provenance(out, "import_gazepoint_folder_v7", "dataset", paste0("Files: ", length(files), "; paired groups: ", length(unique(user_profile$group))), source_files = files)
  attr(out, "validation") <- validate_eye_dataset(out)
  .eye_message("Imported Gazepoint folder with ", length(unique(user_profile$group)), " recording group(s) and ", length(profile$file[profile$export_type == "aoi_statistics"]), " summary report(s).", quiet = quiet)
  out
}

gp_pair_exports <- function(path) {
  if (!dir.exists(path)) .eye_stop("Directory does not exist: ", path)
  files <- list.files(path, pattern = "\\.(csv|txt|tsv)$", full.names = TRUE, ignore.case = TRUE)
  types <- vapply(files, gp_identify_export_type, character(1))
  base <- tools::file_path_sans_ext(basename(files))
  group <- base
  group <- sub("(?i)(?:[_ -](?:all[_ -]?gaze|fixations?|user(?:[_ -]?fix)?))$", "", group, perl = TRUE)
  is_summary <- types == "aoi_statistics"
  group[is_summary] <- "gazepoint_data_summary"
  identity <- lapply(files, .gp_filename_identity)
  data.frame(
    group = group,
    participant_id = vapply(identity, `[[`, character(1), "participant_id"),
    file = normalizePath(files, winslash = "/", mustWork = FALSE),
    export_type = types,
    stringsAsFactors = FALSE
  )
}

gp_audit_file_pairs <- function(path) {
  pairs <- gp_pair_exports(path)
  groups <- split(pairs[pairs$export_type != "aoi_statistics", , drop = FALSE], pairs$group[pairs$export_type != "aoi_statistics"])
  out <- lapply(groups, function(d) data.frame(
    group = d$group[1L], participant_id = d$participant_id[1L],
    has_gaze = any(d$export_type %in% c("gaze", "combined_biometrics")),
    has_fixations = any(d$export_type == "fixations"),
    has_biometrics = any(d$export_type == "combined_biometrics"),
    n_files = nrow(d),
    status = if (any(d$export_type %in% c("gaze", "combined_biometrics"))) "usable" else "incomplete",
    stringsAsFactors = FALSE
  ))
  ans <- if (length(out)) do.call(rbind, out) else data.frame()
  attr(ans, "summary_reports") <- sum(pairs$export_type == "aoi_statistics")
  ans
}


read_gazepoint_biometrics <- function(
    path,
    participant_id = NULL,
    recording_id = NULL,
    session_id = "S001",
    keep_raw = TRUE,
    quiet = FALSE,
    ...) {
  data <- .read_delimited(path, ...)
  nms <- names(data)
  channels <- .gp_biometric_mapping(data)
  if (!length(channels)) .eye_stop("No recognized Gazepoint biometric channels found.")
  has_gaze <- any(c("BPOGX", "FPOGX", "LPOGX", "RPOGX") %in% nms)
  if (has_gaze) {
    out <- read_gazepoint(
      path,
      participant_id = participant_id,
      recording_id = recording_id,
      session_id = session_id,
      keep_raw = keep_raw,
      quiet = TRUE,
      ...
    )
    out$gaze_samples <- empty_eye_table("gaze_samples")
    out$episodes <- empty_eye_table("episodes")
    out$events <- empty_eye_table("events")
    out$responses <- empty_eye_table("responses")
    out$streams <- out$streams[out$streams$stream_type != "gaze_combined", , drop = FALSE]
    out <- add_provenance(out, "select_gazepoint_biometrics", "biometrics|eye_samples", paste(names(channels), collapse = ","), source_files = path)
    attr(out, "validation") <- validate_eye_dataset(out)
    .eye_message("Imported Gazepoint biometric and pupil channels: ", paste(names(channels), collapse = ", "), quiet = quiet)
    return(out)
  }
  identity <- .gp_filename_identity(path, participant_id, recording_id, session_id)
  info <- .gp_time_info(data)
  time_col <- info$tick_col %||% info$media_time_col %||% .gp_pick(nms, "time")
  if (is.null(time_col)) .eye_stop("Cannot identify a Gazepoint time column.")
  data$.eye_dummy_x <- NA_real_
  data$.eye_dummy_y <- NA_real_
  mapping <- eye_mapping(
    timestamp = time_col,
    x = ".eye_dummy_x",
    y = ".eye_dummy_y",
    pupil_left = .gp_pick(nms, "pupil_left"),
    pupil_right = .gp_pick(nms, "pupil_right"),
    pupil_left_valid = .gp_pick(nms, "pupil_left_valid"),
    pupil_right_valid = .gp_pick(nms, "pupil_right_valid"),
    trial = .gp_pick(nms, "trial"),
    stimulus = .gp_pick(nms, "stimulus"),
    biometric_channels = channels
  )
  mapping <- mapping[!vapply(mapping, is.null, logical(1))]
  class(mapping) <- "eye_mapping"
  out <- read_eye_generic(
    data,
    mapping = mapping,
    vendor = "Gazepoint Biometrics",
    participant_id = identity$participant_id,
    recording_id = identity$recording_id,
    session_id = identity$session_id,
    time_unit = if (!is.null(info$tick_col)) "ticks" else "seconds",
    coordinate_space = "display_normalized_top_left",
    pupil_unit = if (any(c("LPMM", "RPMM") %in% nms)) "millimetres" else "vendor_units",
    keep_raw = keep_raw,
    quiet = TRUE
  )
  out <- .gp_apply_timebase(out, data, info)
  out <- .gp_apply_biometric_validity(out, data)
  out$gaze_samples <- empty_eye_table("gaze_samples")
  out$streams <- out$streams[out$streams$stream_type != "gaze_combined", , drop = FALSE]
  out$vendor_metadata$gazepoint_biometrics <- list(source_columns = names(data), channels = channels)
  out <- add_provenance(out, "import_gazepoint_biometrics_v7", "biometrics|eye_samples", paste(names(channels), collapse = ","), source_files = path)
  attr(out, "validation") <- validate_eye_dataset(out)
  .eye_message("Imported Gazepoint biometric channels: ", paste(names(channels), collapse = ", "), quiet = quiet)
  out
}

read_gazepoint_combined <- function(gaze, fixations = NULL, biometrics = NULL, ...) {
  g <- read_gazepoint(gaze, ...)
  xs <- list(g)
  identity <- list(
    participant_id = g$recordings$participant_id[1L],
    recording_id = g$recordings$recording_id[1L],
    session_id = g$recordings$session_id[1L]
  )
  origin <- g$vendor_metadata$gazepoint_timebase$origin_tick %||% NA_real_
  if (!is.null(fixations)) {
    xs[[length(xs) + 1L]] <- read_gazepoint_fixations(
      fixations,
      participant_id = identity$participant_id,
      recording_id = identity$recording_id,
      session_id = identity$session_id,
      origin_tick = origin,
      ...
    )
  }
  if (!is.null(biometrics)) {
    xs[[length(xs) + 1L]] <- read_gazepoint_biometrics(
      biometrics,
      participant_id = identity$participant_id,
      recording_id = identity$recording_id,
      session_id = identity$session_id,
      ...
    )
  }
  out <- do.call(combine_eye_datasets, c(xs, list(resolve_ids = FALSE)))
  attr(out, "validation") <- validate_eye_dataset(out)
  out
}
