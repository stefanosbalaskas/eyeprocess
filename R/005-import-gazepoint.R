.gp_columns <- list(
  time = c("TIME", "TIMETICK", "TIME_TICK", "timestamp", "TIME_SECS"),
  participant = c("USER", "USER_ID", "PARTICIPANT", "participant_id"),
  recording = c("RECORDING_ID", "recording_id", "SESSION_ID"),
  x = c("BPOGX", "FPOGX", "CX", "gaze_x"),
  y = c("BPOGY", "FPOGY", "CY", "gaze_y"),
  valid = c("BPOGV", "FPOGV", "gaze_valid"),
  left_x = c("LPOGX", "left_x"),
  left_y = c("LPOGY", "left_y"),
  right_x = c("RPOGX", "right_x"),
  right_y = c("RPOGY", "right_y"),
  left_valid = c("LPOGV", "left_valid"),
  right_valid = c("RPOGV", "right_valid"),
  pupil_left = c("LPMM", "LPD", "LPS", "pupil_left"),
  pupil_right = c("RPMM", "RPD", "RPS", "pupil_right"),
  pupil_left_valid = c("LPMMV", "LPV", "pupil_left_valid"),
  pupil_right_valid = c("RPMMV", "RPV", "pupil_right_valid"),
  fixation_id = c("FPOGID", "FIXATION_ID", "fixation_id"),
  stimulus = c("MEDIA_ID", "MEDIA_NAME", "stimulus_id"),
  trial = c("TRIAL_ID", "TRIAL_INDEX", "trial_id"),
  marker = c("USER_DATA", "MARKER", "EVENT", "event_name")
)

.gp_pick <- function(nms, key) .first_existing(nms, .gp_columns[[key]])

is_gazepoint_export <- function(path, inspect_rows = 20L) {
  if (dir.exists(path)) {
    files <- list.files(path, pattern = "\\.(csv|txt|tsv)$", full.names = TRUE, ignore.case = TRUE)
    if (!length(files)) return(0)
    return(max(vapply(files, is_gazepoint_export, numeric(1), inspect_rows = inspect_rows)))
  }
  ext <- tolower(tools::file_ext(path))
  if (!ext %in% c("csv", "txt", "tsv")) return(0)
  header <- tryCatch(names(.read_delimited(path, nrows = min(2L, inspect_rows))), error = function(e) character())
  if (!length(header)) return(0)
  gp_hits <- sum(toupper(header) %in% unique(toupper(unlist(.gp_columns, use.names = FALSE))))
  signature <- any(c("FPOGX", "BPOGX", "LPOGX", "RPOGX", "FPOGV", "BPOGV") %in% toupper(header))
  score <- min(1, gp_hits / 8)
  if (signature) score <- max(score, 0.85)
  if (grepl("-user(-fix)?\\.csv$|CurrentAOIStatistics", basename(path), ignore.case = TRUE)) score <- max(score, 0.9)
  score
}

gp_identify_export_type <- function(path) {
  if (dir.exists(path)) return("folder")
  base <- basename(path)
  d <- tryCatch(.read_delimited(path, nrows = 3L), error = function(e) NULL)
  nms <- if (is.null(d)) character() else toupper(names(d))
  if (grepl("CurrentAOIStatistics", base, ignore.case = TRUE)) return("aoi_statistics")
  if (grepl("-fix\\.csv$", base, ignore.case = TRUE)) return("fixations")
  if (any(grepl("^(HR|HEART|GSR|EDA|DIAL|IBI)", nms))) return("combined_biometrics")
  if (all(c("FPOGS", "FPOGD") %in% nms) && !"BPOGX" %in% nms) return("fixations")
  if (any(c("BPOGX", "FPOGX", "LPOGX", "RPOGX") %in% nms)) return("gaze")
  "unknown"
}

gp_profile_export <- function(path) {
  type <- gp_identify_export_type(path)
  files <- if (dir.exists(path)) list.files(path, full.names = TRUE) else path
  details <- lapply(files, function(f) {
    if (dir.exists(f)) return(NULL)
    d <- tryCatch(.read_delimited(f, nrows = 10L), error = function(e) NULL)
    data.frame(
      file = normalizePath(f, winslash = "/", mustWork = FALSE),
      export_type = gp_identify_export_type(f),
      columns = if (is.null(d)) NA_character_ else paste(names(d), collapse = "|"),
      n_columns = if (is.null(d)) NA_integer_ else ncol(d),
      size_bytes = file.info(f)$size,
      confidence = is_gazepoint_export(f),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(.bind_rows_base, details)
  attr(out, "path_type") <- type
  out
}

gp_list_export_fields <- function(path) {
  if (dir.exists(path)) {
    files <- list.files(path, pattern = "\\.(csv|txt|tsv)$", full.names = TRUE, ignore.case = TRUE)
    return(unique(unlist(lapply(files, gp_list_export_fields), use.names = FALSE)))
  }
  names(.read_delimited(path, nrows = 1L))
}

gp_validate_export <- function(path) {
  profile <- gp_profile_export(path)
  issues <- data.frame(
    severity = character(), code = character(), file = character(), message = character(),
    stringsAsFactors = FALSE
  )
  if (!nrow(profile)) {
    issues <- rbind(issues, data.frame(severity = "error", code = "no_files", file = path, message = "No readable files found."))
  }
  low <- which(profile$confidence < 0.5)
  if (length(low)) {
    issues <- rbind(issues, data.frame(
      severity = "warning", code = "low_format_confidence",
      file = profile$file[low], message = "File does not strongly match known Gazepoint fields.",
      stringsAsFactors = FALSE
    ))
  }
  issues
}

.gp_mapping <- function(data) {
  nms <- names(data)
  mapping <- eye_mapping(
    participant = .gp_pick(nms, "participant"),
    recording = .gp_pick(nms, "recording"),
    timestamp = .gp_pick(nms, "time"),
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
    trial = .gp_pick(nms, "trial"),
    stimulus = .gp_pick(nms, "stimulus"),
    event_name = .gp_pick(nms, "marker")
  )
  mapping <- mapping[!vapply(mapping, is.null, logical(1))]
  class(mapping) <- "eye_mapping"
  mapping
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
  if (dir.exists(path)) return(read_gazepoint_folder(path, participant_id = participant_id, session_id = session_id, keep_raw = keep_raw, quiet = quiet, ...))
  type <- gp_identify_export_type(path)
  if (type == "fixations") return(read_gazepoint_fixations(path, participant_id = participant_id, recording_id = recording_id, session_id = session_id, keep_raw = keep_raw, quiet = quiet, ...))
  if (type == "aoi_statistics") return(read_gazepoint_aoi_statistics(path, participant_id = participant_id, recording_id = recording_id, session_id = session_id, keep_raw = keep_raw, quiet = quiet, ...))
  data <- .read_delimited(path, ...)
  mapping <- .gp_mapping(data)
  if (!all(c("timestamp", "x", "y") %in% names(mapping))) {
    .eye_stop("Gazepoint sample export is missing identifiable time and gaze-coordinate columns.")
  }
  pupil_unit <- if (any(c("LPMM", "RPMM", "LPMMV", "RPMMV") %in% names(data))) "millimetres" else "vendor_units"
  mapping$biometric_channels <- .gp_biometric_mapping(data)
  out <- read_eye_generic(
    data,
    mapping = mapping,
    time_unit = "seconds",
    coordinate_space = "display_normalized_top_left",
    screen_width = screen_width,
    screen_height = screen_height,
    pupil_unit = pupil_unit,
    vendor = "Gazepoint",
    recording_id = recording_id,
    participant_id = participant_id,
    session_id = session_id,
    nominal_sampling_rate = nominal_sampling_rate,
    keep_raw = keep_raw,
    quiet = TRUE
  )
  out$recordings$device_model <- "Gazepoint"
  out$recordings$software_name <- "Gazepoint Analysis"
  out$vendor_metadata$gazepoint <- list(
    export_type = type,
    source_columns = names(data),
    source_file = normalizePath(path, winslash = "/", mustWork = FALSE),
    coordinate_convention = "normalized top-left; out-of-range values retained"
  )
  if (isTRUE(keep_raw)) out$raw$gazepoint <- data
  out <- gp_parse_user_events(out)
  out <- gp_parse_media_events(out)
  out <- add_provenance(out, "import_gazepoint", "dataset", paste0("Gazepoint export type: ", type), source_files = path)
  .eye_message("Imported Gazepoint export: ", basename(path), quiet = quiet)
  out
}

read_gazepoint_gaze <- function(...) read_gazepoint(...)

.gp_biometric_mapping <- function(data) {
  nms <- names(data)
  candidates <- list(
    heart_rate = c("HR", "HEART_RATE", "HEARTRATE", "Heart Rate"),
    interbeat_interval = c("IBI", "INTERBEAT_INTERVAL", "RR_INTERVAL"),
    eda = c("GSR", "EDA", "SKIN_CONDUCTANCE", "GSR_RAW"),
    skin_conductance_level = c("GSR_SCL", "SCL"),
    skin_conductance_response = c("GSR_SCR", "SCR"),
    engagement_dial = c("DIAL", "ENGAGEMENT", "ENGAGEMENT_DIAL")
  )
  out <- lapply(candidates, function(x) .first_existing(nms, x))
  out <- out[!vapply(out, is.null, logical(1))]
  unlist(out, use.names = TRUE)
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
  time_col <- .gp_pick(nms, "time")
  if (is.null(time_col)) .eye_stop("Cannot identify a Gazepoint time column.")
  channels <- .gp_biometric_mapping(data)
  if (!length(channels)) .eye_stop("No recognized Gazepoint biometric channels found.")
  dummy_x <- .gp_pick(nms, "x")
  dummy_y <- .gp_pick(nms, "y")
  if (is.null(dummy_x)) { data$.eye_dummy_x <- NA_real_; dummy_x <- ".eye_dummy_x" }
  if (is.null(dummy_y)) { data$.eye_dummy_y <- NA_real_; dummy_y <- ".eye_dummy_y" }
  mapping <- eye_mapping(
    participant = .gp_pick(nms, "participant"),
    recording = .gp_pick(nms, "recording"),
    timestamp = time_col,
    x = dummy_x,
    y = dummy_y,
    trial = .gp_pick(nms, "trial"),
    stimulus = .gp_pick(nms, "stimulus"),
    biometric_channels = channels
  )
  out <- read_eye_generic(
    data, mapping = mapping, vendor = "Gazepoint Biometrics",
    participant_id = participant_id, recording_id = recording_id,
    session_id = session_id, time_unit = "seconds",
    coordinate_space = "display_normalized_top_left",
    keep_raw = keep_raw, quiet = TRUE
  )
  out$gaze_samples <- empty_eye_table("gaze_samples")
  out$streams <- out$streams[out$streams$stream_type != "gaze_combined", , drop = FALSE]
  units <- c(
    heart_rate = "beats_per_minute", interbeat_interval = "milliseconds",
    eda = "vendor_units", skin_conductance_level = "microsiemens",
    skin_conductance_response = "microsiemens", engagement_dial = "vendor_units"
  )
  idx <- match(out$biometrics$channel, names(units))
  out$biometrics$unit[!is.na(idx)] <- unname(units[idx[!is.na(idx)]])
  stream_idx <- match(out$streams$stream_type, names(units))
  out$streams$value_unit[!is.na(stream_idx)] <- unname(units[stream_idx[!is.na(stream_idx)]])
  out$vendor_metadata$gazepoint_biometrics <- list(source_columns = names(data), channels = channels)
  out <- add_provenance(out, "import_gazepoint_biometrics", "biometrics", paste(names(channels), collapse = ","), source_files = path)
  .eye_message("Imported Gazepoint biometrics channels: ", paste(names(channels), collapse = ", "), quiet = quiet)
  out
}

read_gazepoint_fixations <- function(
    path,
    participant_id = NULL,
    recording_id = NULL,
    session_id = "S001",
    keep_raw = TRUE,
    quiet = FALSE,
    ...) {
  data <- .read_delimited(path, ...)
  nms <- names(data)
  pick <- function(...) .first_existing(nms, c(...))
  start <- .safe_numeric(data[[pick("FPOGS", "FIXATION_START", "start_time") %||% names(data)[1L]]])
  duration <- .safe_numeric(.map_column(data, list(duration = pick("FPOGD", "FIXATION_DURATION", "duration_ms", "duration")), "duration"))
  if (all(duration < 20, na.rm = TRUE)) duration_ms <- duration * 1000 else duration_ms <- duration
  end <- start + duration_ms / 1000
  participant <- participant_id %||% .first_nonmissing(data[[pick("USER", "USER_ID", "participant_id") %||% names(data)[1L]]], "P001")
  rec <- recording_id %||% paste0("rec_", participant, "_", session_id)
  fixation_id <- as.character(.map_column(data, list(fixation_id = pick("FPOGID", "FIXATION_ID", "fixation_id")), "fixation_id"))
  missing_id <- is.na(fixation_id) | !nzchar(fixation_id)
  fixation_id[missing_id] <- sprintf("%07d", which(missing_id))
  episodes <- data.frame(
    episode_id = paste0(rec, "_fix_", fixation_id),
    recording_id = rec,
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
    trial_id = .as_character_id(.map_column(data, list(trial = pick("TRIAL_ID", "TRIAL_INDEX", "trial_id")), "trial")),
    stimulus_id = .as_character_id(.map_column(data, list(stimulus = pick("MEDIA_ID", "MEDIA_NAME", "stimulus_id")), "stimulus")),
    aoi_id = .as_character_id(.map_column(data, list(aoi = pick("AOI", "AOI_ID", "aoi_id")), "aoi")),
    stringsAsFactors = FALSE
  )
  recordings <- data.frame(
    recording_id = rec, participant_id = participant, session_id = session_id,
    vendor = "Gazepoint", vendor_family = "Gazepoint", device_model = "Gazepoint",
    firmware_version = NA_character_, software_name = "Gazepoint Analysis",
    software_version = NA_character_, experiment_type = NA_character_,
    nominal_sampling_rate = NA_real_, screen_width_px = NA_real_, screen_height_px = NA_real_,
    recording_start = NA_character_, source_timezone = NA_character_,
    source_file_set = normalizePath(path, winslash = "/", mustWork = FALSE),
    stringsAsFactors = FALSE
  )
  coord <- new_coordinate_space("coord_display_normalized_top_left", "display_normalized_top_left")
  out <- new_eye_dataset(
    recordings = recordings, episodes = episodes, coordinate_spaces = coord,
    raw = if (keep_raw) list(gazepoint_fixations = data) else list(),
    vendor_metadata = list(gazepoint_fixations = list(source_columns = names(data))),
    validate = FALSE
  )
  out <- add_provenance(out, "import_gazepoint_fixations", "episodes", paste0(nrow(episodes), " fixations"), source_files = path)
  .eye_message("Imported ", nrow(episodes), " Gazepoint fixations.", quiet = quiet)
  out
}

read_gazepoint_events <- function(path, ...) {
  x <- read_gazepoint(path, ...)
  x$gaze_samples <- empty_eye_table("gaze_samples")
  x$eye_samples <- empty_eye_table("eye_samples")
  x$biometrics <- empty_eye_table("biometrics")
  x
}

read_gazepoint_combined <- function(gaze, fixations = NULL, biometrics = NULL, ...) {
  xs <- list(read_gazepoint(gaze, ...))
  if (!is.null(fixations)) xs[[length(xs) + 1L]] <- read_gazepoint_fixations(fixations, ...)
  if (!is.null(biometrics)) xs[[length(xs) + 1L]] <- read_gazepoint_biometrics(biometrics, ...)
  do.call(combine_eye_datasets, c(xs, list(resolve_ids = FALSE)))
}

read_gazepoint_folder <- function(
    path,
    include = c("gaze", "fixations", "events", "biometrics", "aoi"),
    participant_id = NULL,
    session_id = "S001",
    keep_raw = TRUE,
    recursive = FALSE,
    quiet = FALSE,
    ...) {
  if (!dir.exists(path)) .eye_stop("Directory does not exist: ", path)
  files <- list.files(path, pattern = "\\.(csv|txt|tsv)$", full.names = TRUE, recursive = recursive, ignore.case = TRUE)
  if (!length(files)) .eye_stop("No delimited Gazepoint exports found.")
  types <- vapply(files, gp_identify_export_type, character(1))
  objs <- list()
  for (i in seq_along(files)) {
    type <- types[i]
    if (type == "gaze" && "gaze" %in% include) objs[[length(objs) + 1L]] <- read_gazepoint(files[i], participant_id = participant_id, session_id = session_id, keep_raw = keep_raw, quiet = TRUE, ...)
    if (type == "combined_biometrics" && any(c("gaze", "biometrics") %in% include)) objs[[length(objs) + 1L]] <- read_gazepoint(files[i], participant_id = participant_id, session_id = session_id, keep_raw = keep_raw, quiet = TRUE, ...)
    if (type == "fixations" && "fixations" %in% include) objs[[length(objs) + 1L]] <- read_gazepoint_fixations(files[i], participant_id = participant_id, session_id = session_id, keep_raw = keep_raw, quiet = TRUE, ...)
    if (type == "aoi_statistics" && "aoi" %in% include) objs[[length(objs) + 1L]] <- read_gazepoint_aoi_statistics(files[i], participant_id = participant_id, session_id = session_id, keep_raw = keep_raw, quiet = TRUE, ...)
  }
  if (!length(objs)) .eye_stop("No requested Gazepoint export types were found.")
  out <- do.call(combine_eye_datasets, c(objs, list(resolve_ids = FALSE)))
  out$vendor_metadata$gazepoint_folder <- gp_pair_exports(path)
  out <- add_provenance(out, "import_gazepoint_folder", "dataset", paste0("Files: ", length(files)), source_files = files)
  .eye_message("Imported Gazepoint folder with ", length(objs), " recognized component(s).", quiet = quiet)
  out
}

gp_pair_exports <- function(path) {
  if (!dir.exists(path)) .eye_stop("Directory does not exist: ", path)
  files <- list.files(path, pattern = "\\.(csv|txt|tsv)$", full.names = TRUE, ignore.case = TRUE)
  types <- vapply(files, gp_identify_export_type, character(1))
  stem <- basename(files)
  stem <- sub("-user-fix\\.csv$", "", stem, ignore.case = TRUE)
  stem <- sub("-user\\.csv$", "", stem, ignore.case = TRUE)
  stem <- sub("\\.(csv|txt|tsv)$", "", stem, ignore.case = TRUE)
  data.frame(group = stem, file = files, export_type = types, stringsAsFactors = FALSE)
}

gp_match_recordings <- function(path) gp_pair_exports(path)
gp_match_biometrics <- function(path) {
  pairs <- gp_pair_exports(path)
  pairs[pairs$export_type %in% c("combined_biometrics", "gaze"), , drop = FALSE]
}

gp_audit_file_pairs <- function(path) {
  pairs <- gp_pair_exports(path)
  groups <- split(pairs, pairs$group)
  do.call(rbind, lapply(groups, function(d) data.frame(
    group = d$group[1L],
    has_gaze = any(d$export_type %in% c("gaze", "combined_biometrics")),
    has_fixations = any(d$export_type == "fixations"),
    has_aoi = any(d$export_type == "aoi_statistics"),
    n_files = nrow(d),
    status = if (any(d$export_type %in% c("gaze", "combined_biometrics"))) "usable" else "incomplete",
    stringsAsFactors = FALSE
  )))
}

read_gazepoint_aoi_statistics <- function(path, participant_id = NULL, recording_id = NULL, session_id = "S001", keep_raw = TRUE, quiet = FALSE, ...) {
  d <- .read_delimited(path, ...)
  participant <- participant_id %||% .first_nonmissing(d[[.first_existing(names(d), c("USER", "participant_id")) %||% names(d)[1L]]], "P001")
  rec <- recording_id %||% paste0("rec_", participant, "_", session_id)
  defs <- data.frame(
    aoi_id = .as_character_id(d[[.first_existing(names(d), c("AOI_ID", "AOI", "AOI Name", "aoi_id")) %||% names(d)[1L]]]),
    aoi_name = as.character(d[[.first_existing(names(d), c("AOI_NAME", "AOI", "AOI Name", "aoi_name")) %||% names(d)[1L]]]),
    stimulus_id = .as_character_id(.map_column(d, list(stimulus = .first_existing(names(d), c("MEDIA_ID", "MEDIA_NAME", "stimulus_id"))), "stimulus")),
    shape_type = "unknown",
    coordinate_space_id = "coord_display_normalized_top_left",
    parent_aoi_id = NA_character_,
    source = "Gazepoint AOI statistics",
    stringsAsFactors = FALSE
  )
  defs <- defs[!duplicated(defs$aoi_id), , drop = FALSE]
  recordings <- data.frame(
    recording_id = rec, participant_id = participant, session_id = session_id,
    vendor = "Gazepoint", vendor_family = "Gazepoint", device_model = "Gazepoint",
    firmware_version = NA_character_, software_name = "Gazepoint Analysis",
    software_version = NA_character_, experiment_type = NA_character_,
    nominal_sampling_rate = NA_real_, screen_width_px = NA_real_, screen_height_px = NA_real_,
    recording_start = NA_character_, source_timezone = NA_character_, source_file_set = path,
    stringsAsFactors = FALSE
  )
  out <- new_eye_dataset(
    recordings = recordings,
    aoi_definitions = defs,
    coordinate_spaces = new_coordinate_space("coord_display_normalized_top_left", "display_normalized_top_left"),
    raw = if (keep_raw) list(gazepoint_aoi_statistics = d) else list(),
    validate = FALSE
  )
  out <- add_provenance(out, "import_gazepoint_aoi_statistics", "aoi_definitions", paste0(nrow(defs), " AOIs"), source_files = path)
  .eye_message("Imported Gazepoint AOI statistics.", quiet = quiet)
  out
}

gp_parse_user_events <- function(x) {
  .assert_eye_dataset(x)
  if (!length(x$raw)) return(x)
  raw <- x$raw$gazepoint %||% x$raw$generic
  if (is.null(raw) || !is.data.frame(raw)) return(x)
  col <- .first_existing(names(raw), c("USER_DATA", "MARKER", "EVENT"))
  time_col <- .gp_pick(names(raw), "time")
  if (is.null(col) || is.null(time_col)) return(x)
  values <- as.character(raw[[col]])
  keep <- !is.na(values) & nzchar(values)
  if (!any(keep)) return(x)
  rec <- if (nrow(x$recordings) == 1L) rep(x$recordings$recording_id, nrow(raw)) else .resolve_recording_vector(raw, .gp_mapping(raw))
  ev <- data.frame(
    event_id = paste0(rec[keep], "_gp_user_", sprintf("%07d", seq_len(sum(keep)))),
    recording_id = rec[keep],
    timestamp_native = .safe_numeric(raw[[time_col]])[keep],
    timestamp_seconds = .safe_numeric(raw[[time_col]])[keep],
    event_type = "user_data",
    event_name = values[keep],
    event_value = values[keep],
    duration = NA_real_, source = "Gazepoint USER_DATA",
    native_record = values[keep], trial_id = NA_character_, stimulus_id = NA_character_,
    stringsAsFactors = FALSE
  )
  if (nrow(x$events)) {
    existing_key <- paste(x$events$recording_id, x$events$timestamp_seconds, x$events$event_name, sep = "\r")
    new_key <- paste(ev$recording_id, ev$timestamp_seconds, ev$event_name, sep = "\r")
    ev <- ev[!new_key %in% existing_key, , drop = FALSE]
  }
  if (nrow(ev)) x$events <- standardize_eye_table(.bind_rows_base(x$events, ev), "events")
  x
}

gp_parse_media_events <- function(x) {
  .assert_eye_dataset(x)
  if (!nrow(x$gaze_samples) || all(is.na(x$gaze_samples$stimulus_id))) return(x)
  d <- x$gaze_samples[order(x$gaze_samples$recording_id, x$gaze_samples$timestamp_seconds), ]
  changed <- ave(as.character(d$stimulus_id), d$recording_id, FUN = function(z) c(TRUE, z[-1L] != z[-length(z)]))
  keep <- as.logical(changed) & !is.na(d$stimulus_id)
  if (!any(keep)) return(x)
  ev <- data.frame(
    event_id = paste0(d$recording_id[keep], "_media_", sprintf("%07d", seq_len(sum(keep)))),
    recording_id = d$recording_id[keep],
    timestamp_native = d$timestamp_native[keep], timestamp_seconds = d$timestamp_seconds[keep],
    event_type = "media_change", event_name = "MEDIA_START",
    event_value = d$stimulus_id[keep], duration = NA_real_, source = "Gazepoint MEDIA_ID",
    native_record = NA_character_, trial_id = d$trial_id[keep], stimulus_id = d$stimulus_id[keep],
    stringsAsFactors = FALSE
  )
  if (nrow(x$events)) {
    existing_key <- paste(x$events$recording_id, x$events$timestamp_seconds, x$events$event_type, x$events$event_value, sep = "\r")
    new_key <- paste(ev$recording_id, ev$timestamp_seconds, ev$event_type, ev$event_value, sep = "\r")
    ev <- ev[!new_key %in% existing_key, , drop = FALSE]
  }
  if (nrow(ev)) x$events <- standardize_eye_table(.bind_rows_base(x$events, ev), "events")
  x
}

gp_reconstruct_trials <- function(x, ...) build_trials(x, ...)
gp_reconstruct_stimuli <- function(x, ...) build_stimulus_intervals(x, ...)
gp_align_media_ids <- function(x) assign_trials(x)
gp_parse_markers <- function(x) gp_parse_user_events(x)

gp_check_sampling_rate <- function(x, ...) audit_sampling_rate(x, expected_hz = 60, ...)
gp_check_validity_fields <- function(x, ...) audit_signal_quality(x, ...)
gp_check_fixation_ids <- function(x) audit_episodes(x, type = "fixation")
gp_check_media_timing <- function(x) audit_event_order(x, event_type = "media_change")
gp_check_pupil_channels <- function(x, ...) audit_pupil_quality(x, ...)
gp_check_biometrics_sync <- function(x, ...) audit_clock_sync(x, ...)
