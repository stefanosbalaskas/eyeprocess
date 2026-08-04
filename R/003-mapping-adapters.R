eye_mapping <- function(
    participant = NULL,
    recording = NULL,
    session = NULL,
    timestamp = NULL,
    timestamp_device = NULL,
    x = NULL,
    y = NULL,
    z = NULL,
    left_x = NULL,
    left_y = NULL,
    right_x = NULL,
    right_y = NULL,
    gaze_valid = NULL,
    left_valid = NULL,
    right_valid = NULL,
    confidence = NULL,
    pupil_left = NULL,
    pupil_right = NULL,
    pupil_left_valid = NULL,
    pupil_right_valid = NULL,
    fixation_id = NULL,
    blink_id = NULL,
    trial = NULL,
    item = NULL,
    stimulus = NULL,
    condition = NULL,
    response = NULL,
    score = NULL,
    response_time = NULL,
    event_name = NULL,
    event_value = NULL,
    event_type = NULL,
    biometric_channels = NULL,
    extra = list()) {
  values <- as.list(environment())
  values$extra <- NULL
  values <- c(values, extra)
  values <- values[!vapply(values, is.null, logical(1))]
  structure(values, class = "eye_mapping")
}

print.eye_mapping <- function(x, ...) {
  cat("<eye_mapping>\n")
  if (!length(x)) {
    cat("  No fields mapped.\n")
  } else {
    for (nm in names(x)) cat("  ", nm, " <- ", paste(x[[nm]], collapse = ", "), "\n", sep = "")
  }
  invisible(x)
}

validate_eye_mapping <- function(mapping, data = NULL, required = c("timestamp", "x", "y")) {
  if (!inherits(mapping, "eye_mapping") && !is.list(mapping)) .eye_stop("`mapping` must be an `eye_mapping` object or list.")
  missing_map <- setdiff(required, names(mapping))
  if (length(missing_map)) .eye_stop("Required mapping field(s) absent: ", paste(missing_map, collapse = ", "), ".")
  if (!is.null(data)) {
    .assert_data_frame(data, "data")
    mapped <- unique(unlist(mapping, use.names = FALSE))
    absent <- setdiff(mapped, names(data))
    if (length(absent)) .eye_stop("Mapped source column(s) absent: ", paste(absent, collapse = ", "), ".")
  }
  invisible(mapping)
}

infer_eye_mapping <- function(data, vendor = NULL) {
  .assert_data_frame(data, "data")
  nms <- names(data)
  pick <- function(...) .first_existing(nms, c(...))
  mapping <- eye_mapping(
    participant = pick("participant_id", "participant", "subject", "subject_id", "user", "USER", "Recording participant"),
    recording = pick("recording_id", "recording", "Recording name", "recording id", "Recording UUID"),
    session = pick("session_id", "session", "session_name"),
    timestamp = pick("timestamp_seconds", "timestamp", "time", "TIME", "TIMETICK", "Recording timestamp", "gaze_timestamp", "world_timestamp", "timestamp [ns]"),
    x = pick("gaze_x", "x", "FPOGX", "BPOGX", "Gaze point X", "Gaze2d x", "norm_pos_x", "x [px]"),
    y = pick("gaze_y", "y", "FPOGY", "BPOGY", "Gaze point Y", "Gaze2d y", "norm_pos_y", "y [px]"),
    gaze_valid = pick("gaze_valid", "valid", "FPOGV", "BPOGV", "Validity left", "Validity right"),
    confidence = pick("confidence", "Confidence"),
    pupil_left = pick("pupil_left", "LPMM", "LPMMV", "Pupil diameter left", "Pupil diameter left [mm]", "diameter_3d_left"),
    pupil_right = pick("pupil_right", "RPMM", "RPMMV", "Pupil diameter right", "Pupil diameter right [mm]", "diameter_3d_right"),
    fixation_id = pick("fixation_id", "FPOGID", "Fixation index", "fixation id"),
    trial = pick("trial_id", "trial", "TRIAL_INDEX", "Trial", "trial number"),
    stimulus = pick("stimulus_id", "stimulus", "MEDIA_ID", "Presented Stimulus name", "world_index"),
    response = pick("response", "response_value", "answer"),
    score = pick("score", "correct", "accuracy"),
    response_time = pick("response_time", "rt", "reaction_time"),
    event_name = pick("event_name", "event", "Event", "USER_DATA", "message"),
    event_value = pick("event_value", "value", "Event value", "message_value")
  )
  mapping <- mapping[!vapply(mapping, is.null, logical(1))]
  class(mapping) <- "eye_mapping"
  mapping
}

register_eye_adapter <- function(name, detect, read, validate = NULL, priority = 0, overwrite = FALSE) {
  .assert_scalar_character(name, "name")
  if (!is.function(detect)) .eye_stop("`detect` must be a function.")
  if (!is.function(read)) .eye_stop("`read` must be a function.")
  if (!is.null(validate) && !is.function(validate)) .eye_stop("`validate` must be NULL or a function.")
  if (!isTRUE(overwrite) && name %in% names(.eye_env$adapters)) .eye_stop("Adapter `", name, "` is already registered.")
  .eye_env$adapters[[name]] <- list(
    name = name, detect = detect, read = read, validate = validate,
    priority = as.numeric(priority), registered_at = .now_utc()
  )
  invisible(name)
}

unregister_eye_adapter <- function(name) {
  .assert_scalar_character(name, "name")
  .eye_env$adapters[[name]] <- NULL
  invisible(name)
}

supported_eye_formats <- function() {
  adapters <- .eye_env$adapters
  if (!length(adapters)) return(data.frame())
  out <- do.call(rbind, lapply(adapters, function(a) data.frame(
    name = a$name,
    priority = a$priority,
    has_validator = !is.null(a$validate),
    registered_at = a$registered_at,
    stringsAsFactors = FALSE
  )))
  out[order(-out$priority, out$name), , drop = FALSE]
}

detect_eye_format <- function(path, inspect_rows = 20L, candidates = NULL) {
  if (!file.exists(path)) .eye_stop("Path does not exist: ", path)
  adapters <- .eye_env$adapters
  if (!is.null(candidates)) adapters <- adapters[intersect(names(adapters), candidates)]
  if (!length(adapters)) .eye_stop("No eye-data adapters are registered.")
  scored <- lapply(adapters, function(a) {
    score <- tryCatch(suppressWarnings(a$detect(path, inspect_rows = inspect_rows)), error = function(e) 0)
    if (is.logical(score)) score <- if (isTRUE(score)) 1 else 0
    score <- suppressWarnings(as.numeric(score)[1L])
    if (!is.finite(score)) score <- 0
    data.frame(format = a$name, confidence = max(0, min(1, score)), priority = a$priority, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, scored)
  out <- out[order(-out$confidence, -out$priority, out$format), , drop = FALSE]
  rownames(out) <- NULL
  class(out) <- c("eye_format_detection", "data.frame")
  out
}

print.eye_format_detection <- function(x, ...) {
  cat("Eye-data format detection\n")
  print.data.frame(x, row.names = FALSE)
  invisible(x)
}

read_eye_export <- function(path, vendor = "auto", confidence_threshold = 0.55, ...) {
  if (identical(vendor, "auto")) {
    detection <- detect_eye_format(path)
    if (!nrow(detection) || detection$confidence[1L] < confidence_threshold) {
      .eye_stop(
        "No adapter reached the required confidence threshold (",
        confidence_threshold, "). Use `read_eye_generic()` with an explicit mapping."
      )
    }
    if (nrow(detection) > 1L && detection$confidence[1L] == detection$confidence[2L]) {
      .eye_warn("Format detection is tied; selecting `", detection$format[1L], "` by adapter priority.")
    }
    vendor <- detection$format[1L]
  }
  if (!vendor %in% names(.eye_env$adapters)) .eye_stop("Unknown adapter `", vendor, "`.")
  out <- .eye_env$adapters[[vendor]]$read(path, ...)
  attr(out, "format_detection") <- if (exists("detection", inherits = FALSE)) detection else NULL
  out
}

read_eye_folder <- function(path, recursive = FALSE, vendor = "auto", pattern = NULL, combine = TRUE, ...) {
  if (!dir.exists(path)) .eye_stop("Directory does not exist: ", path)
  files <- list.files(path, pattern = pattern, full.names = TRUE, recursive = recursive)
  files <- files[!dir.exists(files)]
  if (!length(files)) .eye_stop("No files found in: ", path)
  if (!isTRUE(combine)) return(lapply(files, read_eye_export, vendor = vendor, ...))
  objs <- lapply(files, function(f) tryCatch(read_eye_export(f, vendor = vendor, ...), error = function(e) NULL))
  objs <- objs[vapply(objs, is_eye_dataset, logical(1))]
  if (!length(objs)) .eye_stop("No files in the folder could be imported.")
  do.call(combine_eye_datasets, objs)
}

combine_eye_datasets <- function(..., resolve_ids = TRUE) {
  xs <- list(...)
  if (length(xs) == 1L && is.list(xs[[1L]]) && !is_eye_dataset(xs[[1L]])) xs <- xs[[1L]]
  if (!length(xs) || !all(vapply(xs, is_eye_dataset, logical(1)))) .eye_stop("All inputs must be `eye_dataset` objects.")
  if (isTRUE(resolve_ids)) {
    seen <- character()
    for (i in seq_along(xs)) {
      ids <- unique(stats::na.omit(xs[[i]]$recordings$recording_id))
      duplicate_ids <- intersect(ids, seen)
      if (length(duplicate_ids)) {
        suffix <- paste0("_set", i)
        xs[[i]] <- remap_recording_ids(xs[[i]], setNames(paste0(duplicate_ids, suffix), duplicate_ids))
      }
      seen <- c(seen, unique(stats::na.omit(xs[[i]]$recordings$recording_id)))
    }
  }
  tables <- lapply(canonical_table_names(), function(nm) do.call(.bind_rows_base, lapply(xs, `[[`, nm)))
  names(tables) <- canonical_table_names()
  # Folder-based formats frequently distribute one recording across several
  # files. Identity tables are therefore de-duplicated after binding, while
  # observation tables retain all rows.
  identity_keys <- c(
    recordings = "recording_id",
    streams = "stream_id",
    coordinate_spaces = "coordinate_space_id",
    aoi_definitions = "aoi_id",
    calibrations = "calibration_id"
  )
  for (nm in intersect(names(identity_keys), names(tables))) {
    key <- identity_keys[[nm]]
    if (nrow(tables[[nm]]) && key %in% names(tables[[nm]])) {
      tables[[nm]] <- tables[[nm]][!duplicated(tables[[nm]][[key]]), , drop = FALSE]
      rownames(tables[[nm]]) <- NULL
    }
  }
  raw <- unlist(lapply(xs, `[[`, "raw"), recursive = FALSE)
  metadata <- unlist(lapply(xs, `[[`, "vendor_metadata"), recursive = FALSE)
  out <- do.call(new_eye_dataset, c(tables, list(raw = raw, vendor_metadata = metadata, validate = FALSE)))
  out <- add_provenance(out, "combine_datasets", "dataset", paste0("Combined ", length(xs), " datasets."))
  attr(out, "validation") <- validate_eye_dataset(out)
  out
}

remap_recording_ids <- function(x, mapping) {
  .assert_eye_dataset(x)
  old <- names(mapping)
  new <- unname(mapping)
  for (nm in canonical_table_names()) {
    d <- x[[nm]]
    if ("recording_id" %in% names(d) && nrow(d)) {
      idx <- match(d$recording_id, old)
      replace <- !is.na(idx)
      d$recording_id[replace] <- new[idx[replace]]
      x[[nm]] <- d
    }
  }
  x <- add_provenance(x, "remap_recording_ids", "dataset", paste(paste(old, new, sep = "->"), collapse = ";"))
  x
}
