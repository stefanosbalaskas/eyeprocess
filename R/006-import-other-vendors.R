# Tobii -------------------------------------------------------------------

is_tobii_export <- function(path, inspect_rows = 20L) {
  if (dir.exists(path)) return(0)
  if (!tolower(tools::file_ext(path)) %in% c("tsv", "txt", "csv")) return(0)
  d <- tryCatch(.read_delimited(path, nrows = min(3L, inspect_rows)), error = function(e) NULL)
  if (is.null(d)) return(0)
  nms <- tolower(names(d))
  hits <- sum(grepl("recording timestamp|gaze point|pupil diameter|tobii|eye movement type|presented stimulus", nms))
  signature <- any(grepl("recording timestamp", nms)) && any(grepl("gaze point", nms))
  if (signature) return(max(0.85, min(1, hits / 8)))
  min(0.7, hits / 8)
}

.tobii_valid <- function(x) {
  if (is.logical(x)) return(x)
  z <- tolower(as.character(x))
  out <- rep(NA, length(z))
  out[grepl("valid|^0$", z)] <- TRUE
  out[grepl("invalid|^1$|^2$|^3$|^4$", z)] <- FALSE
  out
}

read_tobii <- function(
    path,
    participant_id = NULL,
    recording_id = NULL,
    session_id = "S001",
    time_unit = "microseconds",
    coordinate_space = "display_pixels_top_left",
    screen_width = NA_real_,
    screen_height = NA_real_,
    keep_raw = TRUE,
    quiet = FALSE,
    ...) {
  d <- .read_delimited(path, ...)
  nms <- names(d)
  pick <- function(...) .first_existing(nms, c(...))
  time_col <- pick("Recording timestamp", "recording_timestamp", "system_time_stamp", "device_time_stamp", "timestamp")
  if (is.null(time_col)) .eye_stop("Cannot identify a Tobii timestamp column.")
  x_col <- pick("Gaze point X", "Gaze point X (MCSnorm)", "gaze_point_x", "Gaze2d x")
  y_col <- pick("Gaze point Y", "Gaze point Y (MCSnorm)", "gaze_point_y", "Gaze2d y")
  lx <- pick("Gaze point left X", "Gaze point left X (MCSnorm)", "left_gaze_point_on_display_area_x")
  ly <- pick("Gaze point left Y", "Gaze point left Y (MCSnorm)", "left_gaze_point_on_display_area_y")
  rx <- pick("Gaze point right X", "Gaze point right X (MCSnorm)", "right_gaze_point_on_display_area_x")
  ry <- pick("Gaze point right Y", "Gaze point right Y (MCSnorm)", "right_gaze_point_on_display_area_y")
  if (is.null(x_col) && !is.null(lx) && !is.null(rx)) {
    d$.eye_tobii_x <- rowMeans(cbind(.safe_numeric(d[[lx]]), .safe_numeric(d[[rx]])), na.rm = TRUE)
    d$.eye_tobii_y <- rowMeans(cbind(.safe_numeric(d[[ly]]), .safe_numeric(d[[ry]])), na.rm = TRUE)
    d$.eye_tobii_x[!is.finite(d$.eye_tobii_x)] <- NA_real_
    d$.eye_tobii_y[!is.finite(d$.eye_tobii_y)] <- NA_real_
    x_col <- ".eye_tobii_x"
    y_col <- ".eye_tobii_y"
  }
  if (is.null(x_col) || is.null(y_col)) {
    d$.eye_tobii_x <- NA_real_
    d$.eye_tobii_y <- NA_real_
    x_col <- ".eye_tobii_x"
    y_col <- ".eye_tobii_y"
  }
  participant_col <- pick("Participant name", "Participant", "participant_id", "subject")
  recording_col <- pick("Recording name", "Recording", "recording_id")
  mapping <- eye_mapping(
    participant = participant_col,
    recording = recording_col,
    timestamp = time_col,
    x = x_col,
    y = y_col,
    left_x = lx,
    left_y = ly,
    right_x = rx,
    right_y = ry,
    pupil_left = pick("Pupil diameter left", "Pupil diameter left [mm]", "left_pupil_diameter"),
    pupil_right = pick("Pupil diameter right", "Pupil diameter right [mm]", "right_pupil_diameter"),
    pupil_left_valid = pick("Validity left", "left_pupil_validity"),
    pupil_right_valid = pick("Validity right", "right_pupil_validity"),
    fixation_id = pick("Fixation index", "Eye movement type index", "fixation_id"),
    trial = pick("Trial", "Trial number", "trial_id"),
    stimulus = pick("Presented Stimulus name", "Presented Media name", "stimulus_id"),
    event_name = pick("Event", "event_name"),
    event_value = pick("Event value", "event_value")
  )
  mapping <- mapping[!vapply(mapping, is.null, logical(1))]
  class(mapping) <- "eye_mapping"
  out <- read_eye_generic(
    d, mapping = mapping, time_unit = time_unit,
    coordinate_space = coordinate_space,
    screen_width = screen_width, screen_height = screen_height,
    pupil_unit = "millimetres", vendor = "Tobii",
    participant_id = participant_id, recording_id = recording_id,
    session_id = session_id, keep_raw = keep_raw, quiet = TRUE
  )
  # Replace validity parsing with Tobii semantics when available.
  lv <- pick("Validity left", "left_gaze_point_validity")
  rv <- pick("Validity right", "right_gaze_point_validity")
  if (!is.null(lv) || !is.null(rv)) {
    valid_l <- if (!is.null(lv)) .tobii_valid(d[[lv]]) else rep(NA, nrow(d))
    valid_r <- if (!is.null(rv)) .tobii_valid(d[[rv]]) else rep(NA, nrow(d))
    out$gaze_samples$valid <- valid_l | valid_r
    out$gaze_samples$valid[is.na(out$gaze_samples$valid)] <- FALSE
  }
  out$recordings$software_name <- "Tobii Pro Lab"
  out$vendor_metadata$tobii <- list(
    source_columns = names(d),
    heterogeneous_rows = TRUE,
    timestamp_role = time_col,
    export_note = "Rows may represent gaze observations or events; temporal order uses timestamps."
  )
  if (keep_raw) out$raw$tobii <- d
  out <- add_provenance(out, "import_tobii", "dataset", paste0(nrow(d), " Tobii rows"), source_files = path)
  .eye_message("Imported Tobii export.", quiet = quiet)
  out
}

# Pupil Labs ---------------------------------------------------------------

is_pupil_labs_export <- function(path, inspect_rows = 20L) {
  if (dir.exists(path)) {
    files <- tolower(list.files(path))
    if (any(files %in% c("gaze.csv", "gaze_positions.csv", "3d_eye_states.csv", "pupil_positions.csv"))) return(0.95)
    return(0)
  }
  base <- tolower(basename(path))
  if (base %in% c("gaze.csv", "fixations.csv", "events.csv", "3d_eye_states.csv", "gaze_positions.csv", "pupil_positions.csv")) return(0.85)
  if (!tolower(tools::file_ext(path)) %in% c("csv", "tsv", "txt")) return(0)
  d <- tryCatch(suppressWarnings(.read_delimited(path, nrows = 2L)), error = function(e) NULL)
  if (is.null(d)) return(0)
  nms <- tolower(names(d))
  hits <- sum(grepl("gaze_timestamp|world_timestamp|norm_pos|worn|azimuth|elevation|section id|recording id", nms))
  min(0.8, hits / 6)
}

pupil_labs_format <- function(path) {
  files <- if (dir.exists(path)) tolower(list.files(path)) else tolower(basename(path))
  if (any(files %in% c("gaze_positions.csv", "pupil_positions.csv"))) return("core")
  if (any(files %in% c("3d_eye_states.csv", "gaze.csv"))) return("neon")
  "unknown"
}

read_pupillabs <- function(path, format = c("auto", "neon", "core"), ...) {
  format <- match.arg(format)
  if (format == "auto") format <- pupil_labs_format(path)
  if (format == "neon") return(read_pupil_neon(path, ...))
  if (format == "core") return(read_pupil_core(path, ...))
  .eye_stop("Could not determine Pupil Labs format. Specify `format = \"neon\"` or `format = \"core\"`.")
}

read_pupil_neon <- function(
    path,
    participant_id = "P001",
    session_id = "S001",
    recording_id = NULL,
    keep_raw = TRUE,
    quiet = FALSE,
    ...) {
  gaze_path <- if (dir.exists(path)) file.path(path, "gaze.csv") else path
  if (!file.exists(gaze_path)) .eye_stop("Pupil Labs Neon `gaze.csv` not found.")
  d <- .read_delimited(gaze_path, ...)
  nms <- names(d)
  pick <- function(...) .first_existing(nms, c(...))
  time_col <- pick("timestamp [ns]", "timestamp_ns", "timestamp", "gaze timestamp [ns]")
  x_col <- pick("gaze x [px]", "x [px]", "gaze_x", "x")
  y_col <- pick("gaze y [px]", "y [px]", "gaze_y", "y")
  if (is.null(time_col) || is.null(x_col) || is.null(y_col)) .eye_stop("Neon gaze export lacks identifiable timestamp/x/y columns.")
  rec_col <- pick("recording id", "recording_id", "Recording UUID")
  mapping <- eye_mapping(
    recording = rec_col,
    timestamp = time_col,
    x = x_col,
    y = y_col,
    confidence = pick("confidence", "worn"),
    fixation_id = pick("fixation id", "fixation_id"),
    stimulus = pick("section id", "section_id", "world_index")
  )
  mapping <- mapping[!vapply(mapping, is.null, logical(1))]
  class(mapping) <- "eye_mapping"
  out <- read_eye_generic(
    d, mapping = mapping, time_unit = "nanoseconds",
    coordinate_space = "world_camera_pixels", vendor = "Pupil Labs Neon",
    participant_id = participant_id, recording_id = recording_id,
    session_id = session_id, keep_raw = keep_raw, quiet = TRUE
  )
  az <- pick("azimuth [deg]", "gaze azimuth [deg]", "azimuth_deg")
  el <- pick("elevation [deg]", "gaze elevation [deg]", "elevation_deg")
  if (!is.null(az)) out$gaze_samples$azimuth_deg <- .safe_numeric(d[[az]])
  if (!is.null(el)) out$gaze_samples$elevation_deg <- .safe_numeric(d[[el]])
  if (dir.exists(path)) {
    out <- .read_neon_companions(out, path, keep_raw = keep_raw)
  }
  out$recordings$device_model <- "Neon"
  out$recordings$software_name <- "Pupil Cloud/Neon"
  out$vendor_metadata$pupil_neon <- list(
    source_columns = names(d),
    timestamp_unit = "UTC nanoseconds",
    coordinate_space = "world camera pixels"
  )
  out <- add_provenance(out, "import_pupil_neon", "dataset", paste0(nrow(d), " gaze rows"), source_files = gaze_path)
  .eye_message("Imported Pupil Labs Neon export.", quiet = quiet)
  out
}

.read_neon_companions <- function(x, folder, keep_raw = TRUE) {
  fix_path <- file.path(folder, "fixations.csv")
  event_path <- file.path(folder, "events.csv")
  eye_path <- file.path(folder, "3d_eye_states.csv")
  if (file.exists(fix_path)) {
    d <- .read_delimited(fix_path)
    nms <- names(d); pick <- function(...) .first_existing(nms, c(...))
    start <- .safe_numeric(d[[pick("start timestamp [ns]", "start_timestamp_ns", "start timestamp") %||% names(d)[1L]]]) * 1e-9
    end <- .safe_numeric(.map_column(d, list(end = pick("end timestamp [ns]", "end_timestamp_ns", "end timestamp")), "end")) * 1e-9
    duration <- .safe_numeric(.map_column(d, list(duration = pick("duration [ms]", "duration_ms", "duration")), "duration"))
    eps <- data.frame(
      episode_id = paste0(x$recordings$recording_id[1L], "_neon_fix_", sprintf("%07d", seq_len(nrow(d)))),
      recording_id = x$recordings$recording_id[1L], episode_type = "fixation", eye = "combined",
      start_time = start, end_time = end, duration_ms = ifelse(is.finite(duration), duration, (end - start) * 1000),
      start_x = NA_real_, start_y = NA_real_, end_x = NA_real_, end_y = NA_real_,
      centroid_x = .safe_numeric(.map_column(d, list(x = pick("fixation x [px]", "x [px]", "x")), "x")),
      centroid_y = .safe_numeric(.map_column(d, list(y = pick("fixation y [px]", "y [px]", "y")), "y")),
      amplitude = NA_real_, peak_velocity = NA_real_, dispersion = NA_real_,
      coordinate_space_id = x$coordinate_spaces$coordinate_space_id[1L],
      source_algorithm = "Pupil Labs Neon", source_parameters = NA_character_, derived_by = "vendor",
      trial_id = NA_character_, stimulus_id = NA_character_, aoi_id = NA_character_,
      stringsAsFactors = FALSE
    )
    x$episodes <- standardize_eye_table(.bind_rows_base(x$episodes, eps), "episodes")
    if (keep_raw) x$raw$neon_fixations <- d
  }
  if (file.exists(event_path)) {
    d <- .read_delimited(event_path)
    nms <- names(d); pick <- function(...) .first_existing(nms, c(...))
    t <- .safe_numeric(d[[pick("timestamp [ns]", "timestamp_ns", "timestamp") %||% names(d)[1L]]]) * 1e-9
    name_col <- pick("name", "event", "event_name") %||% names(d)[min(2L, ncol(d))]
    ev <- data.frame(
      event_id = paste0(x$recordings$recording_id[1L], "_neon_event_", sprintf("%07d", seq_len(nrow(d)))),
      recording_id = x$recordings$recording_id[1L], timestamp_native = t / 1e-9,
      timestamp_seconds = t, event_type = "event", event_name = as.character(d[[name_col]]),
      event_value = as.character(d[[name_col]]), duration = NA_real_, source = "Pupil Labs Neon",
      native_record = NA_character_, trial_id = NA_character_, stimulus_id = NA_character_,
      stringsAsFactors = FALSE
    )
    x$events <- standardize_eye_table(.bind_rows_base(x$events, ev), "events")
    if (keep_raw) x$raw$neon_events <- d
  }
  if (file.exists(eye_path)) {
    d <- .read_delimited(eye_path)
    nms <- names(d); pick <- function(...) .first_existing(nms, c(...))
    time <- .safe_numeric(d[[pick("timestamp [ns]", "timestamp_ns", "timestamp") %||% names(d)[1L]]]) * 1e-9
    rows <- list()
    for (eye in c("left", "right")) {
      diameter <- pick(paste0("pupil diameter ", eye, " [mm]"), paste0("diameter_3d_", eye), paste0("pupil_diameter_", eye))
      if (is.null(diameter)) next
      rows[[eye]] <- data.frame(
        recording_id = x$recordings$recording_id[1L],
        sample_id = paste0(x$recordings$recording_id[1L], "_neon_eye_", eye, "_", sprintf("%09d", seq_len(nrow(d)))),
        timestamp_native = time / 1e-9, timestamp_seconds = time, eye = eye,
        pupil_diameter = .safe_numeric(d[[diameter]]), pupil_unit = "millimetres",
        pupil_valid = is.finite(.safe_numeric(d[[diameter]])), eye_openness = NA_real_,
        gaze_origin_x = NA_real_, gaze_origin_y = NA_real_, gaze_origin_z = NA_real_,
        gaze_origin_valid = NA, corneal_reflection_x = NA_real_, corneal_reflection_y = NA_real_,
        detector_method = "Pupil Labs 3D eye state", confidence = NA_real_,
        trial_id = NA_character_, stimulus_id = NA_character_, stringsAsFactors = FALSE
      )
    }
    if (length(rows)) x$eye_samples <- standardize_eye_table(.bind_rows_base(rows[[1L]], if (length(rows) > 1L) rows[[2L]] else NULL), "eye_samples")
    if (keep_raw) x$raw$neon_eye_states <- d
  }
  x
}

read_pupil_core <- function(
    path,
    participant_id = "P001",
    session_id = "S001",
    recording_id = NULL,
    keep_raw = TRUE,
    quiet = FALSE,
    ...) {
  gaze_path <- if (dir.exists(path)) file.path(path, "gaze_positions.csv") else path
  if (!file.exists(gaze_path)) .eye_stop("Pupil Core `gaze_positions.csv` not found.")
  d <- .read_delimited(gaze_path, ...)
  nms <- names(d); pick <- function(...) .first_existing(nms, c(...))
  time_col <- pick("gaze_timestamp", "world_timestamp", "timestamp")
  x_col <- pick("norm_pos_x", "gaze_x", "x")
  y_col <- pick("norm_pos_y", "gaze_y", "y")
  if (is.null(time_col) || is.null(x_col) || is.null(y_col)) .eye_stop("Pupil Core gaze export lacks identifiable timestamp/x/y columns.")
  mapping <- eye_mapping(
    timestamp = time_col, x = x_col, y = y_col,
    confidence = pick("confidence"),
    fixation_id = pick("fixation_id"), stimulus = pick("world_index")
  )
  mapping <- mapping[!vapply(mapping, is.null, logical(1))]
  class(mapping) <- "eye_mapping"
  out <- read_eye_generic(
    d, mapping = mapping, time_unit = "seconds",
    coordinate_space = "surface_normalized_bottom_left",
    vendor = "Pupil Labs Core", participant_id = participant_id,
    recording_id = recording_id, session_id = session_id,
    keep_raw = keep_raw, quiet = TRUE
  )
  if (dir.exists(path)) out <- .read_core_companions(out, path, keep_raw)
  out$recordings$device_model <- "Pupil Core"
  out$vendor_metadata$pupil_core <- list(
    source_columns = names(d),
    coordinate_note = "Normalized surface coordinates use bottom-left origin."
  )
  out <- add_provenance(out, "import_pupil_core", "dataset", paste0(nrow(d), " gaze rows"), source_files = gaze_path)
  .eye_message("Imported Pupil Labs Core export.", quiet = quiet)
  out
}

.read_core_companions <- function(x, folder, keep_raw = TRUE) {
  ppath <- file.path(folder, "pupil_positions.csv")
  fpath <- file.path(folder, "fixations.csv")
  if (file.exists(ppath)) {
    d <- .read_delimited(ppath)
    nms <- names(d); pick <- function(...) .first_existing(nms, c(...))
    eye_col <- pick("eye_id", "id", "eye")
    eye <- if (!is.null(eye_col)) ifelse(as.character(d[[eye_col]]) %in% c("0", "right", "R"), "right", "left") else "unknown"
    time <- .safe_numeric(d[[pick("pupil_timestamp", "timestamp") %||% names(d)[1L]]])
    diameter_col <- pick("diameter_3d", "diameter", "pupil_diameter")
    es <- data.frame(
      recording_id = x$recordings$recording_id[1L],
      sample_id = paste0(x$recordings$recording_id[1L], "_core_eye_", sprintf("%09d", seq_len(nrow(d)))),
      timestamp_native = time, timestamp_seconds = time, eye = eye,
      pupil_diameter = .safe_numeric(.map_column(d, list(diameter = diameter_col), "diameter")),
      pupil_unit = if (!is.null(diameter_col) && grepl("3d", diameter_col, ignore.case = TRUE)) "millimetres" else "image_pixels",
      pupil_valid = .safe_numeric(.map_column(d, list(confidence = pick("confidence")), "confidence")) > 0,
      eye_openness = NA_real_, gaze_origin_x = NA_real_, gaze_origin_y = NA_real_, gaze_origin_z = NA_real_,
      gaze_origin_valid = NA, corneal_reflection_x = NA_real_, corneal_reflection_y = NA_real_,
      detector_method = as.character(.map_column(d, list(method = pick("method")), "method")),
      confidence = .safe_numeric(.map_column(d, list(confidence = pick("confidence")), "confidence")),
      trial_id = NA_character_, stimulus_id = NA_character_, stringsAsFactors = FALSE
    )
    x$eye_samples <- standardize_eye_table(.bind_rows_base(x$eye_samples, es), "eye_samples")
    if (keep_raw) x$raw$core_pupil_positions <- d
  }
  if (file.exists(fpath)) {
    d <- .read_delimited(fpath)
    nms <- names(d); pick <- function(...) .first_existing(nms, c(...))
    start <- .safe_numeric(d[[pick("start_timestamp", "start_time") %||% names(d)[1L]]])
    duration <- .safe_numeric(.map_column(d, list(duration = pick("duration", "duration_ms")), "duration"))
    if (all(duration < 100, na.rm = TRUE)) duration_ms <- duration * 1000 else duration_ms <- duration
    ep <- data.frame(
      episode_id = paste0(x$recordings$recording_id[1L], "_core_fix_", sprintf("%07d", seq_len(nrow(d)))),
      recording_id = x$recordings$recording_id[1L], episode_type = "fixation", eye = "combined",
      start_time = start, end_time = start + duration_ms / 1000, duration_ms = duration_ms,
      start_x = NA_real_, start_y = NA_real_, end_x = NA_real_, end_y = NA_real_,
      centroid_x = .safe_numeric(.map_column(d, list(x = pick("norm_pos_x", "x")), "x")),
      centroid_y = .safe_numeric(.map_column(d, list(y = pick("norm_pos_y", "y")), "y")),
      amplitude = NA_real_, peak_velocity = NA_real_, dispersion = .safe_numeric(.map_column(d, list(disp = pick("dispersion")), "disp")),
      coordinate_space_id = x$coordinate_spaces$coordinate_space_id[1L],
      source_algorithm = as.character(.map_column(d, list(method = pick("method")), "method")),
      source_parameters = NA_character_, derived_by = "vendor", trial_id = NA_character_,
      stimulus_id = NA_character_, aoi_id = NA_character_, stringsAsFactors = FALSE
    )
    x$episodes <- standardize_eye_table(.bind_rows_base(x$episodes, ep), "episodes")
    if (keep_raw) x$raw$core_fixations <- d
  }
  x
}

# EyeLink -----------------------------------------------------------------

is_eyelink_export <- function(path, inspect_rows = 20L) {
  if (dir.exists(path)) return(0)
  ext <- tolower(tools::file_ext(path))
  if (ext == "edf") return(0.98)
  if (ext == "asc") {
    lines <- tryCatch(readLines(path, n = inspect_rows, warn = FALSE), error = function(e) character())
    if (any(grepl("^(MSG|EFIX|ESACC|EBLINK|START|END|SAMPLES|EVENTS)", trimws(lines)))) return(0.95)
  }
  if (!ext %in% c("csv", "tsv", "txt")) return(0)
  d <- tryCatch(suppressWarnings(.read_delimited(path, nrows = 2L)), error = function(e) NULL)
  if (is.null(d)) return(0)
  nms <- toupper(names(d))
  hits <- sum(grepl("RECORDING_SESSION_LABEL|CURRENT_FIX|IA_LABEL|TRIAL_INDEX|EYELINK|SACCADE|FIXATION", nms))
  min(0.8, hits / 5)
}

read_eyelink_asc <- function(
    path,
    participant_id = "P001",
    session_id = "S001",
    recording_id = NULL,
    coordinate_space = "display_pixels_top_left",
    screen_width = NA_real_,
    screen_height = NA_real_,
    keep_raw = TRUE,
    quiet = FALSE) {
  lines <- readLines(path, warn = FALSE)
  if (!length(lines)) .eye_stop("ASC file is empty.")
  rec <- recording_id %||% paste0("rec_", participant_id, "_", session_id)
  parsed <- .parse_eyelink_asc(lines, rec)
  recordings <- data.frame(
    recording_id = rec, participant_id = participant_id, session_id = session_id,
    vendor = "SR Research", vendor_family = "EyeLink", device_model = "EyeLink",
    firmware_version = NA_character_, software_name = "EyeLink",
    software_version = NA_character_, experiment_type = NA_character_,
    nominal_sampling_rate = estimate_sampling_rate(parsed$gaze_samples$timestamp_seconds),
    screen_width_px = screen_width, screen_height_px = screen_height,
    recording_start = NA_character_, source_timezone = NA_character_, source_file_set = path,
    stringsAsFactors = FALSE
  )
  coord <- new_coordinate_space("coord_eyelink_display", coordinate_space, width = screen_width, height = screen_height)
  if (nrow(parsed$gaze_samples)) parsed$gaze_samples$coordinate_space_id <- coord$coordinate_space_id
  if (nrow(parsed$episodes)) parsed$episodes$coordinate_space_id <- coord$coordinate_space_id
  streams <- data.frame(
    stream_id = paste0(rec, "_gaze"), recording_id = rec, stream_type = "gaze_combined",
    source_device = "EyeLink", source_clock = "tracker", sampling_type = "sampled",
    nominal_rate_hz = recordings$nominal_sampling_rate, observed_rate_hz = recordings$nominal_sampling_rate,
    timestamp_unit = "milliseconds", value_unit = "pixels",
    coordinate_space_id = coord$coordinate_space_id, processing_level = "raw_imported",
    stringsAsFactors = FALSE
  )
  if (nrow(parsed$gaze_samples)) parsed$gaze_samples$stream_id <- streams$stream_id
  out <- new_eye_dataset(
    recordings = recordings, streams = streams,
    gaze_samples = parsed$gaze_samples, eye_samples = parsed$eye_samples,
    episodes = parsed$episodes, events = parsed$events,
    calibrations = parsed$calibrations, coordinate_spaces = coord,
    raw = if (keep_raw) list(eyelink_asc = lines) else list(),
    vendor_metadata = list(eyelink = list(record_types = parsed$record_types)),
    validate = FALSE
  )
  out <- add_provenance(out, "import_eyelink_asc", "dataset", paste0(length(lines), " ASC lines"), source_files = path)
  attr(out, "validation") <- validate_eye_dataset(out)
  .eye_message("Imported EyeLink ASC file.", quiet = quiet)
  out
}

.parse_eyelink_asc <- function(lines, recording_id) {
  gaze_rows <- list(); eye_rows <- list(); episode_rows <- list(); event_rows <- list(); cal_rows <- list()
  gi <- ei <- epi <- evi <- ci <- 0L
  types <- character(length(lines))
  for (i in seq_along(lines)) {
    line <- trimws(lines[i])
    if (!nzchar(line)) next
    tok <- strsplit(line, "[[:space:]]+")[[1L]]
    type <- tok[1L]
    if (grepl("^[0-9]+$", type)) type <- "SAMPLE"
    types[i] <- type
    if (type == "SAMPLE") {
      vals <- suppressWarnings(as.numeric(tok))
      if (length(vals) >= 4L) {
        gi <- gi + 1L
        x <- vals[2L]; y <- vals[3L]; pupil <- vals[4L]
        if (is.na(x) || is.na(y)) next
        gaze_rows[[gi]] <- data.frame(
          recording_id = recording_id, stream_id = paste0(recording_id, "_gaze"),
          sample_id = paste0(recording_id, "_sample_", sprintf("%09d", gi)),
          timestamp_native = vals[1L], timestamp_seconds = vals[1L] / 1000,
          gaze_x = x, gaze_y = y, gaze_z = NA_real_, azimuth_deg = NA_real_, elevation_deg = NA_real_,
          valid = is.finite(x) && is.finite(y), confidence = NA_real_, fixation_id_source = NA_character_,
          blink_id_source = NA_character_, trial_id = NA_character_, stimulus_id = NA_character_,
          coordinate_space_id = NA_character_, stringsAsFactors = FALSE
        )
        ei <- ei + 1L
        eye_rows[[ei]] <- data.frame(
          recording_id = recording_id, sample_id = paste0(recording_id, "_eye_", sprintf("%09d", ei)),
          timestamp_native = vals[1L], timestamp_seconds = vals[1L] / 1000,
          eye = "recorded", pupil_diameter = pupil, pupil_unit = "vendor_units",
          pupil_valid = is.finite(pupil), eye_openness = NA_real_, gaze_origin_x = NA_real_,
          gaze_origin_y = NA_real_, gaze_origin_z = NA_real_, gaze_origin_valid = NA,
          corneal_reflection_x = NA_real_, corneal_reflection_y = NA_real_,
          detector_method = "EyeLink sample", confidence = NA_real_, trial_id = NA_character_, stimulus_id = NA_character_,
          stringsAsFactors = FALSE
        )
      }
    } else if (type %in% c("EFIX", "ESACC", "EBLINK")) {
      vals <- suppressWarnings(as.numeric(tok[-1L]))
      if (length(vals) >= 4L) {
        epi <- epi + 1L
        episode_type <- switch(type, EFIX = "fixation", ESACC = "saccade", EBLINK = "blink")
        start <- vals[2L] / 1000; end <- vals[3L] / 1000; duration <- vals[4L]
        cx <- if (type == "EFIX" && length(vals) >= 6L) vals[5L] else NA_real_
        cy <- if (type == "EFIX" && length(vals) >= 6L) vals[6L] else NA_real_
        episode_rows[[epi]] <- data.frame(
          episode_id = paste0(recording_id, "_", tolower(type), "_", sprintf("%07d", epi)),
          recording_id = recording_id, episode_type = episode_type,
          eye = as.character(tok[2L]), start_time = start, end_time = end,
          duration_ms = duration, start_x = if (type == "ESACC" && length(vals) >= 8L) vals[5L] else NA_real_,
          start_y = if (type == "ESACC" && length(vals) >= 8L) vals[6L] else NA_real_,
          end_x = if (type == "ESACC" && length(vals) >= 8L) vals[7L] else NA_real_,
          end_y = if (type == "ESACC" && length(vals) >= 8L) vals[8L] else NA_real_,
          centroid_x = cx, centroid_y = cy,
          amplitude = if (type == "ESACC" && length(vals) >= 9L) vals[9L] else NA_real_,
          peak_velocity = if (type == "ESACC" && length(vals) >= 10L) vals[10L] else NA_real_,
          dispersion = NA_real_, coordinate_space_id = NA_character_,
          source_algorithm = "EyeLink online parser", source_parameters = NA_character_, derived_by = "vendor",
          trial_id = NA_character_, stimulus_id = NA_character_, aoi_id = NA_character_, stringsAsFactors = FALSE
        )
      }
    } else if (type == "MSG") {
      time <- suppressWarnings(as.numeric(tok[2L]))
      msg <- paste(tok[-c(1L, 2L)], collapse = " ")
      evi <- evi + 1L
      event_rows[[evi]] <- data.frame(
        event_id = paste0(recording_id, "_msg_", sprintf("%07d", evi)), recording_id = recording_id,
        timestamp_native = time, timestamp_seconds = time / 1000,
        event_type = "message", event_name = sub(" .*", "", msg), event_value = msg,
        duration = NA_real_, source = "EyeLink MSG", native_record = line,
        trial_id = NA_character_, stimulus_id = NA_character_, stringsAsFactors = FALSE
      )
      if (grepl("CALIB|VALIDATION|DRIFT", msg, ignore.case = TRUE)) {
        ci <- ci + 1L
        nums <- suppressWarnings(as.numeric(unlist(regmatches(msg, gregexpr("[0-9]+(?:\\.[0-9]+)?", msg, perl = TRUE)))))
        cal_rows[[ci]] <- data.frame(
          calibration_id = paste0(recording_id, "_cal_", sprintf("%05d", ci)), recording_id = recording_id,
          timestamp_seconds = time / 1000,
          calibration_type = if (grepl("DRIFT", msg, ignore.case = TRUE)) "drift" else if (grepl("VALID", msg, ignore.case = TRUE)) "validation" else "calibration",
          eye = NA_character_, point_count = NA_real_, average_error = if (length(nums)) nums[1L] else NA_real_,
          maximum_error = if (length(nums) > 1L) nums[2L] else NA_real_, error_unit = "degrees",
          validation_status = if (grepl("GOOD|OK|SUCCESS", msg, ignore.case = TRUE)) "passed" else NA_character_,
          drift_offset = if (grepl("DRIFT", msg, ignore.case = TRUE) && length(nums)) nums[1L] else NA_real_,
          source_record = msg, stringsAsFactors = FALSE
        )
      }
    } else if (type %in% c("BUTTON", "INPUT", "START", "END")) {
      time <- suppressWarnings(as.numeric(tok[2L]))
      evi <- evi + 1L
      event_rows[[evi]] <- data.frame(
        event_id = paste0(recording_id, "_event_", sprintf("%07d", evi)), recording_id = recording_id,
        timestamp_native = time, timestamp_seconds = time / 1000,
        event_type = tolower(type), event_name = type, event_value = paste(tok[-1L], collapse = " "),
        duration = NA_real_, source = "EyeLink ASC", native_record = line,
        trial_id = NA_character_, stimulus_id = NA_character_, stringsAsFactors = FALSE
      )
    }
  }
  list(
    gaze_samples = if (length(gaze_rows)) do.call(.bind_rows_base, gaze_rows) else empty_eye_table("gaze_samples"),
    eye_samples = if (length(eye_rows)) do.call(.bind_rows_base, eye_rows) else empty_eye_table("eye_samples"),
    episodes = if (length(episode_rows)) do.call(.bind_rows_base, episode_rows) else empty_eye_table("episodes"),
    events = if (length(event_rows)) do.call(.bind_rows_base, event_rows) else empty_eye_table("events"),
    calibrations = if (length(cal_rows)) do.call(.bind_rows_base, cal_rows) else empty_eye_table("calibrations"),
    record_types = table(types[nzchar(types)])
  )
}

read_eyelink_report <- function(path, mapping = NULL, time_unit = "milliseconds", coordinate_space = "display_pixels_top_left", ...) {
  d <- .read_delimited(path)
  if (is.null(mapping)) mapping <- infer_eye_mapping(d, vendor = "EyeLink Data Viewer")
  read_eye_generic(path, mapping = mapping, time_unit = time_unit, coordinate_space = coordinate_space, vendor = "EyeLink Data Viewer", ...)
}

read_eyelink_edf <- function(
    path,
    edf2asc = Sys.which("edf2asc"),
    output = tempfile(fileext = ".asc"),
    keep_asc = FALSE,
    ...) {
  if (!file.exists(path)) .eye_stop("EDF file does not exist: ", path)
  if (!nzchar(edf2asc)) .eye_stop("`edf2asc` executable was not found. Install SR Research EDF2ASC and provide its path.")
  args <- c("-y", shQuote(normalizePath(path, winslash = "/")), shQuote(output))
  status <- system2(edf2asc, args = args, stdout = TRUE, stderr = TRUE)
  exit <- attr(status, "status") %||% 0L
  if (exit != 0L || !file.exists(output)) .eye_stop("EDF2ASC conversion failed: ", paste(status, collapse = "\n"))
  out <- read_eyelink_asc(output, ...)
  out <- add_provenance(out, "convert_edf2asc", "dataset", details = paste0("Converter: ", edf2asc), source_files = path, reversible = FALSE)
  if (!keep_asc) unlink(output)
  out
}

# SMI BeGaze ---------------------------------------------------------------

is_smi_export <- function(path, inspect_rows = 20L) {
  if (dir.exists(path)) return(0)
  ext <- tolower(tools::file_ext(path))
  if (ext == "idf") return(0.5)
  if (!ext %in% c("txt", "csv", "tsv", "asc")) return(0)
  d <- tryCatch(.read_delimited(path, nrows = 3L), error = function(e) NULL)
  if (is.null(d)) return(0)
  nms <- tolower(names(d))
  hits <- sum(grepl("smi|begaze|point of regard|por x|por y|stimulus name|fixation duration|event type", nms))
  if (any(grepl("begaze|smi", nms))) return(max(0.85, hits / 6))
  min(0.75, hits / 6)
}

read_smi <- function(
    path,
    participant_id = NULL,
    recording_id = NULL,
    session_id = "S001",
    time_unit = "microseconds",
    coordinate_space = "display_pixels_top_left",
    keep_raw = TRUE,
    quiet = FALSE,
    ...) {
  if (tolower(tools::file_ext(path)) == "idf") .eye_stop("Direct SMI IDF import is not supported. Export a text/ASCII file from BeGaze first.")
  d <- .read_delimited(path, ...)
  nms <- names(d); pick <- function(...) .first_existing(nms, c(...))
  time_col <- pick("Time", "Timestamp", "Time [ms]", "Time [us]", "time")
  x_col <- pick("Point of Regard X [px]", "POR X [px]", "Mapped gaze data point X", "Gaze X", "x")
  y_col <- pick("Point of Regard Y [px]", "POR Y [px]", "Mapped gaze data point Y", "Gaze Y", "y")
  if (is.null(time_col) || is.null(x_col) || is.null(y_col)) .eye_stop("SMI export lacks identifiable timestamp/x/y fields.")
  mapping <- eye_mapping(
    participant = pick("Participant", "Subject", "participant_id"),
    recording = pick("Recording", "Session", "recording_id"),
    timestamp = time_col, x = x_col, y = y_col,
    left_x = pick("L POR X [px]", "Left POR X [px]"),
    left_y = pick("L POR Y [px]", "Left POR Y [px]"),
    right_x = pick("R POR X [px]", "Right POR X [px]"),
    right_y = pick("R POR Y [px]", "Right POR Y [px]"),
    pupil_left = pick("L Pupil Diameter [mm]", "Left Pupil Diameter", "L Diameter"),
    pupil_right = pick("R Pupil Diameter [mm]", "Right Pupil Diameter", "R Diameter"),
    fixation_id = pick("Fixation Index", "Fixation ID"),
    trial = pick("Trial", "Trial Index"),
    stimulus = pick("Stimulus", "Stimulus Name"),
    event_name = pick("Event", "Event Type", "Message")
  )
  mapping <- mapping[!vapply(mapping, is.null, logical(1))]
  class(mapping) <- "eye_mapping"
  out <- read_eye_generic(
    d, mapping = mapping, time_unit = time_unit,
    coordinate_space = coordinate_space, vendor = "SMI BeGaze",
    participant_id = participant_id, recording_id = recording_id,
    session_id = session_id, pupil_unit = "millimetres",
    keep_raw = keep_raw, quiet = TRUE
  )
  out$recordings$software_name <- "SMI BeGaze"
  out$vendor_metadata$smi <- list(source_columns = names(d), legacy = TRUE, source_type = "BeGaze text export")
  out <- add_provenance(out, "import_smi", "dataset", paste0(nrow(d), " rows"), source_files = path)
  .eye_message("Imported SMI BeGaze text export.", quiet = quiet)
  out
}

read_smi_raw_export <- function(...) read_smi(...)
read_smi_event_export <- function(...) read_smi(...)
read_smi_aoi_export <- function(...) read_smi(...)
