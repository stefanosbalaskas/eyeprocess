.time_multiplier <- function(unit) {
  unit <- tolower(unit %||% "seconds")
  switch(
    unit,
    seconds = 1,
    second = 1,
    s = 1,
    milliseconds = 1e-3,
    millisecond = 1e-3,
    ms = 1e-3,
    microseconds = 1e-6,
    microsecond = 1e-6,
    us = 1e-6,
    nanoseconds = 1e-9,
    nanosecond = 1e-9,
    ns = 1e-9,
    ticks = 1,
    .eye_stop("Unsupported time unit `", unit, "`.")
  )
}

.map_column <- function(data, mapping, key, default = NA) {
  col <- mapping[[key]]
  if (is.null(col) || !length(col)) return(rep(default, nrow(data)))
  if (!col[1L] %in% names(data)) .eye_stop("Mapped column `", col[1L], "` for `", key, "` does not exist.")
  data[[col[1L]]]
}

.make_recordings <- function(data, mapping, vendor, source_file, nominal_sampling_rate = NA_real_, screen_width = NA_real_, screen_height = NA_real_, device_model = NA_character_, software_name = NA_character_, software_version = NA_character_) {
  participant <- .map_column(data, mapping, "participant", "P001")
  recording <- .map_column(data, mapping, "recording", NA_character_)
  session <- .map_column(data, mapping, "session", "S001")
  if (all(is.na(recording) | !nzchar(as.character(recording)))) {
    recording <- paste0("rec_", as.character(participant), "_", as.character(session))
  }
  key <- paste(recording, participant, session, sep = "\r")
  keep <- !duplicated(key)
  data.frame(
    recording_id = .as_character_id(recording[keep]),
    participant_id = .as_character_id(participant[keep]),
    session_id = .as_character_id(session[keep]),
    vendor = as.character(vendor),
    vendor_family = as.character(vendor),
    device_model = as.character(device_model),
    firmware_version = NA_character_,
    software_name = as.character(software_name),
    software_version = as.character(software_version),
    experiment_type = NA_character_,
    nominal_sampling_rate = as.numeric(nominal_sampling_rate),
    screen_width_px = as.numeric(screen_width),
    screen_height_px = as.numeric(screen_height),
    recording_start = NA_character_,
    source_timezone = NA_character_,
    source_file_set = normalizePath(source_file, winslash = "/", mustWork = FALSE),
    stringsAsFactors = FALSE
  )
}

.resolve_recording_vector <- function(data, mapping) {
  participant <- .map_column(data, mapping, "participant", "P001")
  session <- .map_column(data, mapping, "session", "S001")
  recording <- .map_column(data, mapping, "recording", NA_character_)
  missing <- is.na(recording) | !nzchar(as.character(recording))
  recording[missing] <- paste0("rec_", participant[missing], "_", session[missing])
  .as_character_id(recording)
}

.make_streams <- function(recordings, coordinate_space_id, time_unit, eyes = character(), pupil_unit = NA_character_, vendor = "generic") {
  base <- data.frame(
    stream_id = paste0(recordings$recording_id, "_gaze"),
    recording_id = recordings$recording_id,
    stream_type = "gaze_combined",
    source_device = vendor,
    source_clock = "native",
    sampling_type = "sampled",
    nominal_rate_hz = recordings$nominal_sampling_rate,
    observed_rate_hz = NA_real_,
    timestamp_unit = time_unit,
    value_unit = NA_character_,
    coordinate_space_id = coordinate_space_id,
    processing_level = "raw_imported",
    stringsAsFactors = FALSE
  )
  eyes <- intersect(c("left", "right"), unique(as.character(eyes)))
  if (length(eyes)) {
    pupil <- do.call(.bind_rows_base, lapply(eyes, function(eye) {
      out <- base
      out$stream_id <- paste0(out$recording_id, "_pupil_", eye)
      out$stream_type <- paste0("pupil_", eye)
      out$value_unit <- pupil_unit
      out$coordinate_space_id <- NA_character_
      out
    }))
    base <- .bind_rows_base(base, pupil)
  }
  base
}

.make_biometric_streams <- function(biometrics, recordings, time_unit, vendor = "generic") {
  if (!nrow(biometrics)) return(data.frame())
  keys <- unique(biometrics[c("recording_id", "stream_id", "channel", "unit")])
  rates <- vapply(seq_len(nrow(keys)), function(i) {
    z <- biometrics$timestamp_seconds[
      biometrics$recording_id == keys$recording_id[i] & biometrics$stream_id == keys$stream_id[i]
    ]
    estimate_sampling_rate(z)
  }, numeric(1))
  data.frame(
    stream_id = keys$stream_id,
    recording_id = keys$recording_id,
    stream_type = keys$channel,
    source_device = vendor,
    source_clock = "native",
    sampling_type = "sampled",
    nominal_rate_hz = NA_real_,
    observed_rate_hz = rates,
    timestamp_unit = time_unit,
    value_unit = keys$unit,
    coordinate_space_id = NA_character_,
    processing_level = "raw_imported",
    stringsAsFactors = FALSE
  )
}


read_eye_generic <- function(
    path,
    mapping = NULL,
    delimiter = NULL,
    time_unit = "seconds",
    coordinate_space = "display_normalized_top_left",
    screen_width = NA_real_,
    screen_height = NA_real_,
    pupil_unit = NA_character_,
    vendor = "generic",
    recording_id = NULL,
    participant_id = NULL,
    session_id = "S001",
    nominal_sampling_rate = NA_real_,
    keep_raw = TRUE,
    keep_extra = TRUE,
    encoding = "UTF-8",
    quiet = FALSE,
    ...) {
  source_path <- if (is.data.frame(path)) NA_character_ else as.character(path)
  data <- if (is.data.frame(path)) path else .read_delimited(path, delimiter = delimiter, encoding = encoding, ...)
  if (!nrow(data)) .eye_stop("Input contains no rows.")
  if (is.null(mapping)) mapping <- infer_eye_mapping(data, vendor = vendor)
  if (!inherits(mapping, "eye_mapping")) class(mapping) <- c("eye_mapping", class(mapping))
  if (!is.null(recording_id)) {
    data$.eye_recording_id <- recording_id
    mapping$recording <- ".eye_recording_id"
  }
  if (!is.null(participant_id)) {
    data$.eye_participant_id <- participant_id
    mapping$participant <- ".eye_participant_id"
  }
  if (!is.null(session_id)) {
    data$.eye_session_id <- session_id
    mapping$session <- ".eye_session_id"
  }
  validate_eye_mapping(mapping, data, required = c("timestamp", "x", "y"))
  source_label <- if (is.na(source_path)) "<data.frame>" else normalizePath(source_path, winslash = "/", mustWork = FALSE)
  coord_id <- paste0("coord_", gsub("[^A-Za-z0-9]+", "_", coordinate_space))
  coordinates <- new_coordinate_space(
    coord_id,
    space_type = if (coordinate_space %in% c(
      "display_normalized_top_left", "display_pixels_top_left",
      "surface_normalized_bottom_left", "world_camera_pixels",
      "reference_image_pixels", "user_coordinates_3d",
      "headset_coordinates_3d", "gaze_direction_vector", "custom"
    )) coordinate_space else "custom",
    width = screen_width,
    height = screen_height,
    reference_object = if (grepl("display", coordinate_space)) "display" else NA_character_
  )
  recordings <- .make_recordings(
    data, mapping, vendor = vendor, source_file = source_label,
    nominal_sampling_rate = nominal_sampling_rate,
    screen_width = screen_width, screen_height = screen_height
  )
  rec_vec <- .resolve_recording_vector(data, mapping)
  time_native <- .safe_numeric(.map_column(data, mapping, "timestamp"))
  multiplier <- .time_multiplier(time_unit)
  time_seconds <- time_native * multiplier
  stream_id <- paste0(rec_vec, "_gaze")
  valid <- .safe_logical(.map_column(data, mapping, "gaze_valid", TRUE))
  if (all(is.na(valid))) valid <- rep(TRUE, nrow(data))
  gaze <- data.frame(
    recording_id = rec_vec,
    stream_id = stream_id,
    sample_id = paste0(rec_vec, "_sample_", sprintf("%09d", ave(seq_len(nrow(data)), rec_vec, FUN = seq_along))),
    timestamp_native = time_native,
    timestamp_seconds = time_seconds,
    gaze_x = .safe_numeric(.map_column(data, mapping, "x")),
    gaze_y = .safe_numeric(.map_column(data, mapping, "y")),
    gaze_z = .safe_numeric(.map_column(data, mapping, "z")),
    azimuth_deg = NA_real_,
    elevation_deg = NA_real_,
    valid = valid,
    confidence = .safe_numeric(.map_column(data, mapping, "confidence")),
    fixation_id_source = .as_character_id(.map_column(data, mapping, "fixation_id")),
    blink_id_source = .as_character_id(.map_column(data, mapping, "blink_id")),
    trial_id = .as_character_id(.map_column(data, mapping, "trial")),
    stimulus_id = .as_character_id(.map_column(data, mapping, "stimulus")),
    coordinate_space_id = coord_id,
    stringsAsFactors = FALSE
  )
  eye_samples <- .make_eye_samples_from_mapping(data, mapping, rec_vec, time_native, time_seconds, pupil_unit)
  responses <- .make_responses_from_mapping(data, mapping, rec_vec)
  events <- .make_events_from_mapping(data, mapping, rec_vec, time_native, time_seconds)
  biometrics <- .make_biometrics_from_mapping(data, mapping, rec_vec, time_native, time_seconds)
  streams <- .make_streams(
    recordings, coord_id, time_unit,
    eyes = unique(eye_samples$eye), pupil_unit = pupil_unit, vendor = vendor
  )
  streams$observed_rate_hz[streams$stream_type == "gaze_combined"] <- vapply(
    recordings$recording_id,
    function(id) estimate_sampling_rate(time_seconds[rec_vec == id]),
    numeric(1)
  )
  streams <- .bind_rows_base(streams, .make_biometric_streams(biometrics, recordings, time_unit, vendor))
  raw <- if (isTRUE(keep_raw)) list(generic = data) else list()
  metadata <- list(generic = list(
    mapping = mapping,
    source_columns = names(data),
    source_file = source_label,
    time_unit = time_unit,
    coordinate_space = coordinate_space,
    keep_extra = keep_extra
  ))
  out <- new_eye_dataset(
    recordings = recordings,
    streams = streams,
    gaze_samples = gaze,
    eye_samples = eye_samples,
    events = events,
    responses = responses,
    biometrics = biometrics,
    coordinate_spaces = coordinates,
    raw = raw,
    vendor_metadata = metadata,
    validate = FALSE
  )
  out <- add_provenance(
    out, "import_generic", "dataset",
    details = paste0("Imported ", nrow(data), " rows; vendor=", vendor, "."),
    source_files = source_label,
    reversible = isTRUE(keep_raw)
  )
  attr(out, "validation") <- validate_eye_dataset(out)
  .eye_message("Imported ", nrow(gaze), " gaze samples into ", nrow(recordings), " recording(s).", quiet = quiet)
  out
}

.make_eye_samples_from_mapping <- function(data, mapping, rec_vec, time_native, time_seconds, pupil_unit) {
  rows <- list()
  for (eye in c("left", "right")) {
    pupil_key <- paste0("pupil_", eye)
    x_key <- paste0(eye, "_x")
    y_key <- paste0(eye, "_y")
    valid_key <- paste0("pupil_", eye, "_valid")
    gaze_valid_key <- paste0(eye, "_valid")
    has_any <- any(c(pupil_key, x_key, y_key, valid_key, gaze_valid_key) %in% names(mapping))
    if (!has_any) next
    pupil <- .safe_numeric(.map_column(data, mapping, pupil_key))
    pvalid <- .safe_logical(.map_column(data, mapping, valid_key, NA))
    if (all(is.na(pvalid))) pvalid <- is.finite(pupil)
    rows[[eye]] <- data.frame(
      recording_id = rec_vec,
      sample_id = paste0(rec_vec, "_eye_", eye, "_", sprintf("%09d", ave(seq_len(nrow(data)), rec_vec, FUN = seq_along))),
      timestamp_native = time_native,
      timestamp_seconds = time_seconds,
      eye = eye,
      pupil_diameter = pupil,
      pupil_unit = pupil_unit,
      pupil_valid = pvalid,
      eye_openness = NA_real_,
      gaze_origin_x = .safe_numeric(.map_column(data, mapping, paste0(eye, "_origin_x"))),
      gaze_origin_y = .safe_numeric(.map_column(data, mapping, paste0(eye, "_origin_y"))),
      gaze_origin_z = .safe_numeric(.map_column(data, mapping, paste0(eye, "_origin_z"))),
      gaze_origin_valid = .safe_logical(.map_column(data, mapping, gaze_valid_key, NA)),
      corneal_reflection_x = NA_real_,
      corneal_reflection_y = NA_real_,
      detector_method = NA_character_,
      confidence = .safe_numeric(.map_column(data, mapping, "confidence")),
      trial_id = .as_character_id(.map_column(data, mapping, "trial")),
      stimulus_id = .as_character_id(.map_column(data, mapping, "stimulus")),
      stringsAsFactors = FALSE
    )
  }
  if (!length(rows)) return(empty_eye_table("eye_samples"))
  .bind_rows_base(rows[[1L]], if (length(rows) > 1L) rows[[2L]] else NULL)
}

.make_events_from_mapping <- function(data, mapping, rec_vec, time_native, time_seconds) {
  if (!"event_name" %in% names(mapping)) return(empty_eye_table("events"))
  event_name <- as.character(.map_column(data, mapping, "event_name"))
  keep <- !is.na(event_name) & nzchar(event_name)
  if (!any(keep)) return(empty_eye_table("events"))
  data.frame(
    event_id = paste0(rec_vec[keep], "_event_", sprintf("%07d", seq_len(sum(keep)))),
    recording_id = rec_vec[keep],
    timestamp_native = time_native[keep],
    timestamp_seconds = time_seconds[keep],
    event_type = as.character(.map_column(data, mapping, "event_type", "marker"))[keep],
    event_name = event_name[keep],
    event_value = as.character(.map_column(data, mapping, "event_value"))[keep],
    duration = NA_real_,
    source = "generic_import",
    native_record = NA_character_,
    trial_id = .as_character_id(.map_column(data, mapping, "trial"))[keep],
    stimulus_id = .as_character_id(.map_column(data, mapping, "stimulus"))[keep],
    stringsAsFactors = FALSE
  )
}

.make_responses_from_mapping <- function(data, mapping, rec_vec) {
  if (!any(c("response", "score", "response_time") %in% names(mapping))) return(empty_eye_table("responses"))
  trial <- .as_character_id(.map_column(data, mapping, "trial"))
  item <- .as_character_id(.map_column(data, mapping, "item"))
  participant <- .as_character_id(.map_column(data, mapping, "participant", "P001"))
  response <- as.character(.map_column(data, mapping, "response"))
  score <- .safe_numeric(.map_column(data, mapping, "score"))
  rt <- .safe_numeric(.map_column(data, mapping, "response_time"))
  keep <- !is.na(response) | is.finite(score) | is.finite(rt)
  if (!any(keep)) return(empty_eye_table("responses"))
  tmp <- data.frame(
    recording_id = rec_vec[keep], participant_id = participant[keep],
    trial_id = trial[keep], item_id = item[keep], response = response[keep],
    score = score[keep], response_time = rt[keep], stringsAsFactors = FALSE
  )
  key <- interaction(tmp[c("recording_id", "trial_id", "item_id")], drop = TRUE, lex.order = TRUE)
  tmp <- tmp[!duplicated(key, fromLast = TRUE), , drop = FALSE]
  tmp$response_id <- paste0(tmp$recording_id, "_response_", sprintf("%07d", seq_len(nrow(tmp))))
  tmp$response_timestamp <- NA_real_
  tmp$response_type <- ifelse(is.finite(tmp$score), "scored", "observed")
  tmp$valid_response <- TRUE
  tmp[c("response_id", "recording_id", "participant_id", "trial_id", "item_id", "response", "score", "response_time", "response_timestamp", "response_type", "valid_response")]
}

.canonical_biometric_unit <- function(channel) {
  channel <- tolower(trimws(as.character(channel)))
  units <- c(
    heart_rate = "beats_per_minute",
    interbeat_interval = "milliseconds",
    eda = "microsiemens",
    gsr = "microsiemens",
    skin_conductance = "microsiemens",
    skin_conductance_level = "microsiemens",
    skin_conductance_response = "microsiemens",
    engagement_dial = "vendor_units"
  )
  out <- unname(units[channel])
  out[is.na(out) | !nzchar(out)] <- "vendor_units"
  out
}

.make_biometrics_from_mapping <- function(data, mapping, rec_vec, time_native, time_seconds) {
  channels <- mapping$biometric_channels
  if (is.null(channels) || !length(channels)) return(empty_eye_table("biometrics"))
  if (is.null(names(channels))) .eye_stop("`biometric_channels` must be a named vector/list mapping canonical channels to source columns.")
  rows <- lapply(names(channels), function(channel) {
    col <- channels[[channel]]
    if (!col %in% names(data)) return(NULL)
    value <- .safe_numeric(data[[col]])
    data.frame(
      recording_id = rec_vec,
      stream_id = paste0(rec_vec, "_", channel),
      timestamp_native = time_native,
      timestamp_seconds = time_seconds,
      channel = channel,
      value = value,
      unit = .canonical_biometric_unit(channel),
      valid = is.finite(value),
      processing_level = "raw_imported",
      source_device = NA_character_,
      trial_id = .as_character_id(.map_column(data, mapping, "trial")),
      stimulus_id = .as_character_id(.map_column(data, mapping, "stimulus")),
      stringsAsFactors = FALSE
    )
  })
  do.call(.bind_rows_base, rows)
}
