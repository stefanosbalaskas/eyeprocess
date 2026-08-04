eye_schema <- function(version = .eye_env$schema_version) {
  list(
    version = version,
    tables = list(
      recordings = c(
        "recording_id", "participant_id", "session_id", "vendor",
        "vendor_family", "device_model", "firmware_version", "software_name",
        "software_version", "experiment_type", "nominal_sampling_rate",
        "screen_width_px", "screen_height_px", "recording_start",
        "source_timezone", "source_file_set"
      ),
      streams = c(
        "stream_id", "recording_id", "stream_type", "source_device",
        "source_clock", "sampling_type", "nominal_rate_hz", "observed_rate_hz",
        "timestamp_unit", "value_unit", "coordinate_space_id", "processing_level"
      ),
      gaze_samples = c(
        "recording_id", "stream_id", "sample_id", "timestamp_native",
        "timestamp_seconds", "gaze_x", "gaze_y", "gaze_z", "azimuth_deg",
        "elevation_deg", "valid", "confidence", "fixation_id_source",
        "blink_id_source", "trial_id", "stimulus_id", "coordinate_space_id"
      ),
      eye_samples = c(
        "recording_id", "sample_id", "timestamp_native", "timestamp_seconds",
        "eye", "pupil_diameter", "pupil_unit", "pupil_valid", "eye_openness",
        "gaze_origin_x", "gaze_origin_y", "gaze_origin_z", "gaze_origin_valid",
        "corneal_reflection_x", "corneal_reflection_y", "detector_method",
        "confidence", "trial_id", "stimulus_id"
      ),
      episodes = c(
        "episode_id", "recording_id", "episode_type", "eye", "start_time",
        "end_time", "duration_ms", "start_x", "start_y", "end_x", "end_y",
        "centroid_x", "centroid_y", "amplitude", "peak_velocity", "dispersion",
        "coordinate_space_id", "source_algorithm", "source_parameters",
        "derived_by", "trial_id", "stimulus_id", "aoi_id"
      ),
      events = c(
        "event_id", "recording_id", "timestamp_native", "timestamp_seconds",
        "event_type", "event_name", "event_value", "duration", "source",
        "native_record", "trial_id", "stimulus_id"
      ),
      intervals = c(
        "interval_id", "recording_id", "interval_type", "start_time", "end_time",
        "trial_id", "participant_id", "item_id", "stimulus_id", "condition_id",
        "parent_interval_id", "valid_interval"
      ),
      responses = c(
        "response_id", "recording_id", "participant_id", "trial_id", "item_id",
        "response", "score", "response_time", "response_timestamp",
        "response_type", "valid_response"
      ),
      coordinate_spaces = c(
        "coordinate_space_id", "space_type", "origin", "x_unit", "y_unit",
        "width", "height", "reference_object", "parent_space_id",
        "transform_to_parent", "clipping_policy"
      ),
      aoi_definitions = c(
        "aoi_id", "aoi_name", "stimulus_id", "shape_type",
        "coordinate_space_id", "parent_aoi_id", "source"
      ),
      aoi_geometry = c(
        "aoi_id", "valid_from", "valid_to", "frame_id", "x", "y", "width",
        "height", "polygon", "visible", "coordinate_space_id"
      ),
      biometrics = c(
        "recording_id", "stream_id", "timestamp_native", "timestamp_seconds",
        "channel", "value", "unit", "valid", "processing_level",
        "source_device", "trial_id", "stimulus_id"
      ),
      calibrations = c(
        "calibration_id", "recording_id", "timestamp_seconds", "calibration_type",
        "eye", "point_count", "average_error", "maximum_error", "error_unit",
        "validation_status", "drift_offset", "source_record"
      ),
      features = c(
        "feature_id", "recording_id", "participant_id", "trial_id", "item_id",
        "stimulus_id", "aoi_id", "feature_name", "value", "unit", "level",
        "window_start", "window_end", "observed_fraction", "method",
        "parameters", "derived_at"
      ),
      quality = c(
        "quality_id", "recording_id", "trial_id", "stream_id", "metric",
        "value", "threshold", "status", "message", "computed_at"
      ),
      provenance = c(
        "provenance_id", "timestamp", "action", "component", "details",
        "source_files", "file_hashes", "software", "software_version",
        "reversible", "warnings"
      )
    )
  )
}

schema_table <- function(name, schema = eye_schema()) {
  .assert_scalar_character(name, "name")
  if (!name %in% names(schema$tables)) {
    .eye_stop("Unknown schema table `", name, "`.")
  }
  schema$tables[[name]]
}

empty_eye_table <- function(name, schema = eye_schema()) {
  cols <- schema_table(name, schema)
  out <- setNames(replicate(length(cols), logical(0), simplify = FALSE), cols)
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  class(out) <- c(paste0("eye_", name), "data.frame")
  out
}

standardize_eye_table <- function(data, name, keep_extra = TRUE, schema = eye_schema()) {
  .assert_data_frame(data, "data")
  cols <- schema_table(name, schema)
  for (nm in setdiff(cols, names(data))) data[[nm]] <- rep(NA, nrow(data))
  ordered <- if (isTRUE(keep_extra)) c(cols, setdiff(names(data), cols)) else cols
  data <- data[ordered]
  rownames(data) <- NULL
  class(data) <- c(paste0("eye_", name), "data.frame")
  data
}

validate_eye_table <- function(data, name, strict = FALSE, schema = eye_schema()) {
  .assert_data_frame(data, "data")
  cols <- schema_table(name, schema)
  missing <- setdiff(cols, names(data))
  extra <- setdiff(names(data), cols)
  issues <- data.frame(
    severity = character(), code = character(), table = character(),
    field = character(), message = character(), stringsAsFactors = FALSE
  )
  if (length(missing)) {
    issues <- rbind(issues, data.frame(
      severity = if (strict) "error" else "warning",
      code = "missing_schema_field", table = name, field = missing,
      message = paste0("Schema field `", missing, "` is absent."),
      stringsAsFactors = FALSE
    ))
  }
  if (strict && length(extra)) {
    issues <- rbind(issues, data.frame(
      severity = "warning", code = "extra_field", table = name, field = extra,
      message = paste0("Non-canonical field `", extra, "` is retained."),
      stringsAsFactors = FALSE
    ))
  }
  issues
}

canonical_table_names <- function() names(eye_schema()$tables)

new_coordinate_space <- function(
    coordinate_space_id,
    space_type = c(
      "display_normalized_top_left", "display_pixels_top_left",
      "surface_normalized_bottom_left", "world_camera_pixels",
      "reference_image_pixels", "user_coordinates_3d",
      "headset_coordinates_3d", "gaze_direction_vector", "custom"
    ),
    origin = NULL,
    x_unit = NULL,
    y_unit = NULL,
    width = NA_real_,
    height = NA_real_,
    reference_object = NA_character_,
    parent_space_id = NA_character_,
    transform_to_parent = NA_character_,
    clipping_policy = "retain") {
  space_type <- match.arg(space_type)
  defaults <- switch(
    space_type,
    display_normalized_top_left = list("top_left", "normalized", "normalized"),
    display_pixels_top_left = list("top_left", "pixels", "pixels"),
    surface_normalized_bottom_left = list("bottom_left", "normalized", "normalized"),
    world_camera_pixels = list("top_left", "pixels", "pixels"),
    reference_image_pixels = list("top_left", "pixels", "pixels"),
    user_coordinates_3d = list("vendor_defined", "millimetres", "millimetres"),
    headset_coordinates_3d = list("vendor_defined", "metres", "metres"),
    gaze_direction_vector = list("origin", "unit_vector", "unit_vector"),
    custom = list("unknown", "unknown", "unknown")
  )
  data.frame(
    coordinate_space_id = as.character(coordinate_space_id),
    space_type = space_type,
    origin = origin %||% defaults[[1L]],
    x_unit = x_unit %||% defaults[[2L]],
    y_unit = y_unit %||% defaults[[3L]],
    width = as.numeric(width), height = as.numeric(height),
    reference_object = as.character(reference_object),
    parent_space_id = as.character(parent_space_id),
    transform_to_parent = as.character(transform_to_parent),
    clipping_policy = as.character(clipping_policy),
    stringsAsFactors = FALSE
  )
}
