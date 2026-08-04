new_eye_dataset <- function(
    recordings = NULL,
    streams = NULL,
    gaze_samples = NULL,
    eye_samples = NULL,
    episodes = NULL,
    events = NULL,
    intervals = NULL,
    responses = NULL,
    coordinate_spaces = NULL,
    aoi_definitions = NULL,
    aoi_geometry = NULL,
    biometrics = NULL,
    calibrations = NULL,
    features = NULL,
    quality = NULL,
    provenance = NULL,
    raw = list(),
    vendor_metadata = list(),
    schema_version = .eye_env$schema_version,
    validate = TRUE) {
  tables <- list(
    recordings = recordings,
    streams = streams,
    gaze_samples = gaze_samples,
    eye_samples = eye_samples,
    episodes = episodes,
    events = events,
    intervals = intervals,
    responses = responses,
    coordinate_spaces = coordinate_spaces,
    aoi_definitions = aoi_definitions,
    aoi_geometry = aoi_geometry,
    biometrics = biometrics,
    calibrations = calibrations,
    features = features,
    quality = quality,
    provenance = provenance
  )
  for (nm in names(tables)) {
    if (is.null(tables[[nm]])) tables[[nm]] <- empty_eye_table(nm)
    tables[[nm]] <- standardize_eye_table(tables[[nm]], nm, keep_extra = TRUE)
  }
  out <- c(
    tables,
    list(
      raw = raw,
      vendor_metadata = vendor_metadata,
      schema_version = schema_version
    )
  )
  class(out) <- "eye_dataset"
  if (isTRUE(validate)) {
    report <- validate_eye_dataset(out, strict = FALSE)
    attr(out, "validation") <- report
  }
  out
}

is_eye_dataset <- function(x) inherits(x, "eye_dataset")

as_eye_dataset <- function(x, ...) UseMethod("as_eye_dataset")

as_eye_dataset.eye_dataset <- function(x, ...) x

as_eye_dataset.data.frame <- function(x, mapping = NULL, ...) {
  read_eye_generic(x, mapping = mapping, ...)
}

as_eye_dataset.default <- function(x, ...) {
  .eye_stop("No `as_eye_dataset()` method for class: ", paste(class(x), collapse = "/"), ".")
}

print.eye_dataset <- function(x, ...) {
  counts <- vapply(canonical_table_names(), function(nm) nrow(x[[nm]]), integer(1))
  vendors <- unique(stats::na.omit(x$recordings$vendor))
  cat("<eye_dataset schema ", x$schema_version, ">\n", sep = "")
  cat("  Vendors:      ", if (length(vendors)) paste(vendors, collapse = ", ") else "unspecified", "\n", sep = "")
  cat("  Recordings:   ", counts[["recordings"]], "\n", sep = "")
  cat("  Participants: ", length(unique(stats::na.omit(x$recordings$participant_id))), "\n", sep = "")
  cat("  Streams:      ", counts[["streams"]], "\n", sep = "")
  cat("  Gaze samples: ", counts[["gaze_samples"]], "\n", sep = "")
  cat("  Eye samples:  ", counts[["eye_samples"]], "\n", sep = "")
  cat("  Episodes:     ", counts[["episodes"]], "\n", sep = "")
  cat("  Events:       ", counts[["events"]], "\n", sep = "")
  cat("  Trials:       ", sum(x$intervals$interval_type == "trial", na.rm = TRUE), "\n", sep = "")
  cat("  Responses:    ", counts[["responses"]], "\n", sep = "")
  cat("  Biometrics:   ", counts[["biometrics"]], "\n", sep = "")
  cat("  Features:     ", counts[["features"]], "\n", sep = "")
  invisible(x)
}

summary.eye_dataset <- function(object, ...) {
  x <- object
  quality_status <- if (nrow(x$quality)) table(x$quality$status, useNA = "ifany") else integer()
  out <- list(
    schema_version = x$schema_version,
    vendors = unique(stats::na.omit(x$recordings$vendor)),
    table_rows = vapply(canonical_table_names(), function(nm) nrow(x[[nm]]), integer(1)),
    participants = length(unique(stats::na.omit(x$recordings$participant_id))),
    recordings = nrow(x$recordings),
    trials = sum(x$intervals$interval_type == "trial", na.rm = TRUE),
    coordinate_spaces = unique(stats::na.omit(x$coordinate_spaces$space_type)),
    biometric_channels = unique(stats::na.omit(x$biometrics$channel)),
    feature_names = unique(stats::na.omit(x$features$feature_name)),
    quality_status = quality_status,
    validation = validate_eye_dataset(x, strict = FALSE)
  )
  class(out) <- "summary.eye_dataset"
  out
}

print.summary.eye_dataset <- function(x, ...) {
  cat("eyeprocess dataset summary\n")
  cat("Schema:       ", x$schema_version, "\n", sep = "")
  cat("Vendors:      ", if (length(x$vendors)) paste(x$vendors, collapse = ", ") else "unspecified", "\n", sep = "")
  cat("Participants: ", x$participants, "\n", sep = "")
  cat("Recordings:   ", x$recordings, "\n", sep = "")
  cat("Trials:       ", x$trials, "\n", sep = "")
  cat("Validation:   ", nrow(x$validation), " issue(s)\n", sep = "")
  invisible(x)
}

validate_eye_dataset <- function(x, strict = FALSE, stop_on_error = FALSE) {
  .assert_eye_dataset(x)
  issues <- data.frame(
    severity = character(), code = character(), table = character(),
    field = character(), message = character(), stringsAsFactors = FALSE
  )
  for (nm in canonical_table_names()) {
    issues <- rbind(issues, validate_eye_table(x[[nm]], nm, strict = strict))
  }
  primary_keys <- list(
    recordings = "recording_id",
    streams = "stream_id",
    gaze_samples = "sample_id",
    eye_samples = c("recording_id", "sample_id", "eye"),
    episodes = "episode_id",
    events = "event_id",
    intervals = "interval_id",
    responses = "response_id",
    coordinate_spaces = "coordinate_space_id",
    aoi_definitions = "aoi_id",
    calibrations = "calibration_id",
    features = "feature_id",
    quality = "quality_id",
    provenance = "provenance_id"
  )
  for (nm in names(primary_keys)) {
    d <- x[[nm]]
    keys <- primary_keys[[nm]]
    if (!nrow(d) || !all(keys %in% names(d))) next

    missing_key <- Reduce(`|`, lapply(d[keys], function(z) {
      is.na(z) | !nzchar(trimws(as.character(z)))
    }))
    if (any(missing_key)) {
      issues <- rbind(issues, data.frame(
        severity = "error", code = "missing_primary_key", table = nm,
        field = paste(keys, collapse = "+"),
        message = paste0(
          "Missing values found in primary key `",
          paste(keys, collapse = " + "), "`."
        ),
        stringsAsFactors = FALSE
      ))
    }

    complete <- !missing_key
    if (any(complete)) {
      key_values <- lapply(d[complete, keys, drop = FALSE], function(z) {
        z <- as.character(z)
        z[is.na(z)] <- "<NA>"
        paste0(nchar(z), ":", z)
      })
      composite <- do.call(paste, c(key_values, sep = "|"))
      if (anyDuplicated(composite)) {
        issues <- rbind(issues, data.frame(
          severity = "error", code = "duplicate_primary_key", table = nm,
          field = paste(keys, collapse = "+"),
          message = paste0(
            "Duplicate values found for primary key `",
            paste(keys, collapse = " + "), "`."
          ),
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  recording_ids <- unique(x$recordings$recording_id)
  child_tables <- c("streams", "gaze_samples", "eye_samples", "episodes", "events", "intervals", "responses", "biometrics", "calibrations", "features", "quality")
  for (nm in child_tables) {
    d <- x[[nm]]
    if (nrow(d) && "recording_id" %in% names(d) && length(recording_ids)) {
      orphan <- setdiff(unique(stats::na.omit(d$recording_id)), recording_ids)
      if (length(orphan)) {
        issues <- rbind(issues, data.frame(
          severity = "error", code = "orphan_recording_id", table = nm,
          field = "recording_id",
          message = paste0("Unknown recording id(s): ", paste(orphan, collapse = ", "), "."),
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  stream_ids <- unique(stats::na.omit(x$streams$stream_id))
  for (nm in c("gaze_samples", "biometrics")) {
    d <- x[[nm]]
    if (nrow(d) && "stream_id" %in% names(d)) {
      orphan <- setdiff(unique(stats::na.omit(d$stream_id)), stream_ids)
      if (length(orphan)) {
        issues <- rbind(issues, data.frame(
          severity = "error", code = "orphan_stream_id", table = nm,
          field = "stream_id", message = paste0("Unknown stream id(s): ", paste(orphan, collapse = ", "), "."),
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  coordinate_ids <- unique(stats::na.omit(x$coordinate_spaces$coordinate_space_id))
  for (nm in c("streams", "gaze_samples", "episodes", "aoi_definitions", "aoi_geometry")) {
    d <- x[[nm]]
    if (nrow(d) && "coordinate_space_id" %in% names(d)) {
      orphan <- setdiff(unique(stats::na.omit(d$coordinate_space_id)), coordinate_ids)
      if (length(orphan)) {
        issues <- rbind(issues, data.frame(
          severity = "error", code = "orphan_coordinate_space", table = nm,
          field = "coordinate_space_id", message = paste0("Unknown coordinate space(s): ", paste(orphan, collapse = ", "), "."),
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  if (nrow(x$gaze_samples)) {
    bad_time <- !is.na(x$gaze_samples$timestamp_seconds) & !is.finite(x$gaze_samples$timestamp_seconds)
    if (any(bad_time)) {
      issues <- rbind(issues, data.frame(
        severity = "error", code = "nonfinite_timestamp", table = "gaze_samples",
        field = "timestamp_seconds", message = "Non-finite normalized timestamps detected.",
        stringsAsFactors = FALSE
      ))
    }
  }
  if (nrow(x$intervals)) {
    bad <- is.finite(x$intervals$start_time) & is.finite(x$intervals$end_time) & x$intervals$end_time < x$intervals$start_time
    if (any(bad)) {
      issues <- rbind(issues, data.frame(
        severity = "error", code = "negative_interval", table = "intervals",
        field = "end_time", message = "At least one interval ends before it starts.",
        stringsAsFactors = FALSE
      ))
    }
  }
  rownames(issues) <- NULL
  class(issues) <- c("eye_validation", "data.frame")
  if (isTRUE(stop_on_error) && any(issues$severity == "error")) {
    .eye_stop("`eye_dataset` validation failed with ", sum(issues$severity == "error"), " error(s).")
  }
  issues
}

print.eye_validation <- function(x, ...) {
  if (!nrow(x)) {
    cat("No validation issues detected.\n")
    return(invisible(x))
  }
  cat("eyeprocess validation report\n")
  print.data.frame(x, row.names = FALSE)
  invisible(x)
}

get_eye_table <- function(x, table) {
  .assert_eye_dataset(x)
  .assert_scalar_character(table, "table")
  if (!table %in% names(x)) .eye_stop("Unknown component `", table, "`.")
  x[[table]]
}

set_eye_table <- function(x, table, value, validate = TRUE) {
  .assert_eye_dataset(x)
  .assert_scalar_character(table, "table")
  if (!table %in% canonical_table_names()) .eye_stop("Unknown canonical table `", table, "`.")
  x[[table]] <- standardize_eye_table(value, table, keep_extra = TRUE)
  x <- add_provenance(x, action = "set_table", component = table, details = paste0("Rows: ", nrow(value)))
  if (isTRUE(validate)) attr(x, "validation") <- validate_eye_dataset(x)
  x
}

append_eye_table <- function(x, table, value, validate = TRUE) {
  old <- get_eye_table(x, table)
  set_eye_table(x, table, .bind_rows_base(old, value), validate = validate)
}

add_provenance <- function(
    x,
    action,
    component = NA_character_,
    details = NA_character_,
    source_files = NA_character_,
    file_hashes = NULL,
    software = "eyeprocess",
    software_version = NULL,
    reversible = TRUE,
    warnings = NA_character_) {
  .assert_eye_dataset(x)
  source_files <- paste(as.character(source_files), collapse = "|")
  if (is.null(file_hashes) && !is.na(source_files) && nzchar(source_files)) {
    paths <- strsplit(source_files, "|", fixed = TRUE)[[1L]]
    file_hashes <- paste(.file_md5(paths), collapse = "|")
  }
  if (is.null(software_version)) {
    software_version <- tryCatch(as.character(utils::packageVersion("eyeprocess")), error = function(e) "development")
  }
  row <- data.frame(
    provenance_id = .next_id("prov"),
    timestamp = .now_utc(),
    action = as.character(action),
    component = as.character(component),
    details = as.character(details),
    source_files = source_files,
    file_hashes = paste(as.character(file_hashes), collapse = "|"),
    software = as.character(software),
    software_version = as.character(software_version),
    reversible = isTRUE(reversible),
    warnings = paste(as.character(warnings), collapse = " | "),
    stringsAsFactors = FALSE
  )
  x$provenance <- standardize_eye_table(.bind_rows_base(x$provenance, row), "provenance")
  x
}

provenance_manifest <- function(x) {
  .assert_eye_dataset(x)
  list(
    schema_version = x$schema_version,
    created = .now_utc(),
    sources = unique(stats::na.omit(x$provenance$source_files)),
    file_hashes = unique(stats::na.omit(x$provenance$file_hashes)),
    actions = x$provenance,
    coordinate_spaces = x$coordinate_spaces,
    streams = x$streams,
    validation = validate_eye_dataset(x)
  )
}

compact_eye_dataset <- function(x, drop_raw = FALSE, drop_empty = FALSE) {
  .assert_eye_dataset(x)
  if (isTRUE(drop_raw)) x$raw <- list()
  if (isTRUE(drop_empty)) {
    attr(x, "empty_components") <- canonical_table_names()[vapply(x[canonical_table_names()], nrow, integer(1)) == 0L]
  }
  x <- add_provenance(x, "compact_dataset", "dataset", paste0("drop_raw=", drop_raw, ";drop_empty=", drop_empty), reversible = !drop_raw)
  x
}
