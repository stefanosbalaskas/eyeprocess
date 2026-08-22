# Interoperability, Eye-Tracking-BIDS, and disk-backed storage -----------------

.ep_sanitize_id <- function(x, prefix = "id") {
  x <- as.character(x)
  x[is.na(x) | !nzchar(x)] <- prefix
  x <- gsub("[^A-Za-z0-9]", "", x)
  x[!nzchar(x)] <- prefix
  x
}

.ep_write_json <- function(x, path, pretty = TRUE) {
  .require_namespace("jsonlite", "to write JSON metadata")
  jsonlite::write_json(x, path, auto_unbox = TRUE, pretty = pretty, null = "null", na = "null")
  invisible(path)
}

.ep_read_json <- function(path) {
  .require_namespace("jsonlite", "to read JSON metadata")
  jsonlite::read_json(path, simplifyVector = TRUE)
}

.ep_write_tsv_gz_no_header <- function(x, path) {
  con <- gzfile(path, open = "wt")
  on.exit(close(con), add = TRUE)
  utils::write.table(
    x, con, sep = "\t", row.names = FALSE, col.names = FALSE,
    quote = FALSE, na = "n/a", fileEncoding = "UTF-8"
  )
  invisible(path)
}

.ep_sampling_rate <- function(x, recording_id) {
  s <- x$streams[x$streams$recording_id == recording_id, , drop = FALSE]
  values <- c(s$observed_rate_hz, s$nominal_rate_hz)
  values <- values[is.finite(values) & values > 0]
  if (length(values)) values[[1L]] else NA_real_
}

.ep_first_text <- function(x, default = "unknown") {
  z <- as.character(x)
  z <- z[!is.na(z) & nzchar(trimws(z))]
  if (length(z)) z[[1L]] else default
}

.ep_timestamp_unit <- function(x, recording_id) {
  s <- x$streams[x$streams$recording_id == recording_id & grepl("gaze", x$streams$stream_type, ignore.case = TRUE), , drop = FALSE]
  .ep_first_text(s$timestamp_unit, "unknown")
}

.ep_timestamp_to_seconds <- function(value, unit = "unknown", sampling_rate = NA_real_) {
  value <- suppressWarnings(as.numeric(value))
  unit <- tolower(trimws(as.character(unit %||% "unknown")))[1L]
  multiplier <- switch(unit,
    s = 1, sec = 1, second = 1, seconds = 1,
    ms = 1e-3, millisecond = 1e-3, milliseconds = 1e-3,
    us = 1e-6, microsecond = 1e-6, microseconds = 1e-6,
    ns = 1e-9, nanosecond = 1e-9, nanoseconds = 1e-9,
    NA_real_
  )
  if (is.finite(multiplier) && any(is.finite(value))) return(value * multiplier)
  if (is.finite(sampling_rate) && sampling_rate > 0) return((seq_along(value) - 1) / sampling_rate)
  value
}

.ep_coordinate_metadata <- function(x, recording_id) {
  gaze <- x$gaze_samples[x$gaze_samples$recording_id == recording_id, , drop = FALSE]
  coord_id <- if (nrow(gaze)) gaze$coordinate_space_id[[1L]] else NA_character_
  cs <- x$coordinate_spaces[x$coordinate_spaces$coordinate_space_id == coord_id, , drop = FALSE]
  if (!nrow(cs)) {
    return(list(system = "custom", x_unit = "unknown", y_unit = "unknown"))
  }
  system <- if (grepl("display|screen", cs$space_type[[1L]], ignore.case = TRUE)) {
    "gaze-on-screen"
  } else if (grepl("world", cs$space_type[[1L]], ignore.case = TRUE)) {
    "gaze-in-world"
  } else if (grepl("head|user|direction", cs$space_type[[1L]], ignore.case = TRUE)) {
    "eye-in-head"
  } else {
    "custom"
  }
  list(system = system, x_unit = as.character(cs$x_unit[[1L]]), y_unit = as.character(cs$y_unit[[1L]]))
}

#' Specify disk-backed eyeprocess storage
#'
#' @param path Storage directory or RDS file.
#' @param format One of `"rds"`, `"parquet"`, or `"arrow_dataset"`.
#' @param tables Canonical tables to store.
#' @param partitioning Optional Arrow partition columns.
#' @param compression Parquet compression codec. For writes, the default
#'   `"zstd"` is preferred; when the argument is omitted and that codec
#'   is unavailable, storage falls back to `"snappy"` and then
#'   `"uncompressed"`. An explicitly requested unavailable codec errors.
#' @return An `eye_storage_spec` object.
#' @export
eye_storage_spec <- function(
    path,
    format = c("rds", "parquet", "arrow_dataset"),
    tables = canonical_table_names(),
    partitioning = NULL,
    compression = "zstd") {
  format <- match.arg(format)
  tables <- intersect(as.character(tables), canonical_table_names())
  if (!length(tables)) .eye_stop("No canonical tables were selected for storage.")
  structure(
    list(
      path = normalizePath(path, winslash = "/", mustWork = FALSE),
      format = format,
      tables = tables,
      partitioning = partitioning,
      compression = compression
    ),
    class = "eye_storage_spec"
  )
}

.ep_arrow_resolve_compression <- function(
    compression,
    allow_fallback = FALSE,
    codec_available = NULL) {
  codec <- tolower(trimws(as.character(compression)))
  if (length(codec) != 1L || is.na(codec) || !nzchar(codec)) {
    .eye_stop("`compression` must be one non-empty codec name.")
  }
  if (is.null(codec_available)) {
    .require_namespace(
      "arrow",
      "to determine available Parquet compression codecs"
    )
    codec_available <- arrow::codec_is_available
  }
  available <- function(x) {
    isTRUE(
      tryCatch(
        codec_available(x),
        error = function(e) FALSE
      )
    )
  }
  if (available(codec)) return(codec)
  if (!isTRUE(allow_fallback)) {
    .eye_stop(
      "Arrow compression codec `",
      codec,
      "` is unavailable in this Arrow build."
    )
  }
  if (available("snappy")) return("snappy")
  "uncompressed"
}

#' Write an eye dataset to RDS or Arrow/Parquet storage
#'
#' @param x An `eye_dataset`.
#' @param path Output path.
#' @param format Storage format.
#' @param tables Canonical tables to write.
#' @param partitioning Optional partition columns for Arrow datasets.
#' @param compression Parquet compression codec. For writes, the default
#'   `"zstd"` is preferred; when the argument is omitted and that codec
#'   is unavailable, storage falls back to `"snappy"` and then
#'   `"uncompressed"`. An explicitly requested unavailable codec errors.
#' @param overwrite Whether to replace an existing target.
#' @param retain_metadata Whether to retain raw/vendor metadata in a sidecar RDS.
#' @return An `eye_storage` handle.
#' @export
write_eye_storage <- function(
    x,
    path,
    format = c("rds", "parquet", "arrow_dataset"),
    tables = canonical_table_names(),
    partitioning = NULL,
    compression = "zstd",
    overwrite = FALSE,
    retain_metadata = TRUE) {
  compression_defaulted <- missing(compression)
  .assert_eye_dataset(x)
  format <- match.arg(format)
  spec <- eye_storage_spec(path, format, tables, partitioning, compression)
  exists_target <- file.exists(spec$path) || dir.exists(spec$path)
  if (exists_target && !isTRUE(overwrite)) .eye_stop("Storage target already exists: ", spec$path)
  if (exists_target && isTRUE(overwrite)) unlink(spec$path, recursive = TRUE, force = TRUE)

  if (format == "rds") {
    dir.create(dirname(spec$path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(x, spec$path, version = 3)
    handle <- list(spec = spec, manifest = data.frame(table = "eye_dataset", path = spec$path, rows = NA_integer_))
  } else {
    .require_namespace("arrow", "for Parquet and Arrow dataset storage")
    compression <- .ep_arrow_resolve_compression(
      compression,
      allow_fallback = compression_defaulted
    )
    spec$compression <- compression
    dir.create(spec$path, recursive = TRUE, showWarnings = FALSE)
    manifest <- lapply(spec$tables, function(nm) {
      d <- x[[nm]]
      table_path <- file.path(spec$path, nm)
      if (format == "parquet") {
        table_path <- paste0(table_path, ".parquet")
        arrow::write_parquet(d, table_path, compression = spec$compression)
      } else {
        dir.create(table_path, recursive = TRUE, showWarnings = FALSE)
        part <- intersect(as.character(partitioning), names(d))
        arrow::write_dataset(
          d, table_path, format = "parquet",
          partitioning = if (length(part)) part else NULL,
          compression = spec$compression,
          existing_data_behavior = "overwrite"
        )
      }
      data.frame(table = nm, path = normalizePath(table_path, winslash = "/", mustWork = TRUE), rows = nrow(d))
    })
    manifest <- do.call(rbind, manifest)
    utils::write.csv(manifest, file.path(spec$path, "storage-manifest.csv"), row.names = FALSE)
    saveRDS(
      list(
        schema_version = x$schema_version,
        raw = if (isTRUE(retain_metadata)) x$raw else list(),
        vendor_metadata = if (isTRUE(retain_metadata)) x$vendor_metadata else list()
      ),
      file.path(spec$path, "storage-metadata.rds"),
      version = 3
    )
    handle <- list(spec = spec, manifest = manifest)
  }
  class(handle) <- "eye_storage"
  handle
}

#' Open an eyeprocess storage handle
#'
#' @param path Storage path.
#' @param format Optional explicit format.
#' @return An `eye_storage` handle without collecting all tables.
#' @export
open_eye_storage <- function(path, format = NULL) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (is.null(format)) {
    format <- if (dir.exists(path)) {
      manifest <- file.path(path, "storage-manifest.csv")
      if (!file.exists(manifest)) .eye_stop("Storage manifest is missing from: ", path)
      paths <- utils::read.csv(manifest, stringsAsFactors = FALSE)$path
      if (all(grepl("\\.parquet$", paths))) "parquet" else "arrow_dataset"
    } else {
      "rds"
    }
  }
  spec <- eye_storage_spec(path, format)
  manifest <- if (format == "rds") {
    data.frame(table = "eye_dataset", path = path, rows = NA_integer_, stringsAsFactors = FALSE)
  } else {
    utils::read.csv(file.path(path, "storage-manifest.csv"), stringsAsFactors = FALSE)
  }
  structure(list(spec = spec, manifest = manifest), class = "eye_storage")
}

#' Collect a disk-backed eye dataset
#'
#' @param x An `eye_storage` handle or storage path.
#' @param tables Optional subset of canonical tables.
#' @return An `eye_dataset`.
#' @export
collect_eye_storage <- function(x, tables = NULL) {
  if (is.character(x)) x <- open_eye_storage(x)
  if (!inherits(x, "eye_storage")) .eye_stop("Expected an `eye_storage` handle or path.")
  if (x$spec$format == "rds") return(readRDS(x$spec$path))
  .require_namespace("arrow", "to collect Parquet and Arrow datasets")
  selected <- if (is.null(tables)) x$manifest$table else intersect(as.character(tables), x$manifest$table)
  table_list <- setNames(vector("list", length(selected)), selected)
  for (nm in selected) {
    p <- x$manifest$path[match(nm, x$manifest$table)]
    table_list[[nm]] <- if (x$spec$format == "parquet") {
      as.data.frame(arrow::read_parquet(p))
    } else {
      as.data.frame(arrow::open_dataset(p))
    }
  }
  metadata_path <- file.path(x$spec$path, "storage-metadata.rds")
  metadata <- if (file.exists(metadata_path)) readRDS(metadata_path) else list()
  args <- c(table_list, list(
    raw = metadata$raw %||% list(),
    vendor_metadata = metadata$vendor_metadata %||% list(),
    schema_version = metadata$schema_version %||% .eye_env$schema_version,
    validate = TRUE
  ))
  do.call(new_eye_dataset, args)
}

#' @export
print.eye_storage <- function(x, ...) {
  cat("<eye_storage>\n")
  cat("  Format: ", x$spec$format, "\n", sep = "")
  cat("  Path:   ", x$spec$path, "\n", sep = "")
  cat("  Tables: ", paste(x$manifest$table, collapse = ", "), "\n", sep = "")
  invisible(x)
}

#' Export Eye-Tracking-BIDS physiological recordings
#'
#' Writes BIDS-compatible `_physio.tsv.gz` and JSON pairs with `PhysioType =
#' "eyetrack"`. Continuous files contain no header; `Columns` is stored in the
#' sidecar. Each recorded eye is written separately.
#'
#' @param x An `eye_dataset`.
#' @param path BIDS dataset root.
#' @param task Task label.
#' @param dataset_name Dataset name for `dataset_description.json`.
#' @param overwrite Whether to replace existing files.
#' @param screen_distance_m Optional screen distance in metres.
#' @param screen_size_m Optional two-element screen size in metres.
#' @return A manifest of written files.
#' @export
export_eye_bids <- function(
    x,
    path,
    task = "task",
    dataset_name = "eyeprocess eye-tracking dataset",
    overwrite = FALSE,
    screen_distance_m = NULL,
    screen_size_m = NULL) {
  .assert_eye_dataset(x)
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (dir.exists(path) && length(list.files(path, all.files = TRUE, no.. = TRUE)) && !isTRUE(overwrite)) {
    .eye_stop("BIDS output directory is not empty: ", path)
  }
  if (dir.exists(path) && isTRUE(overwrite)) unlink(path, recursive = TRUE, force = TRUE)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  task <- .ep_sanitize_id(task, "task")
  .ep_write_json(
    list(Name = dataset_name, BIDSVersion = "1.11.1", DatasetType = "raw",
         GeneratedBy = list(list(Name = "eyeprocess", Version = as.character(utils::packageVersion("eyeprocess"))))),
    file.path(path, "dataset_description.json")
  )

  participants <- unique(x$recordings[c("participant_id")])
  original_participants <- participants$participant_id
  participants$participant_id <- paste0("sub-", .ep_sanitize_id(participants$participant_id, "unknown"))
  if (anyDuplicated(participants$participant_id)) {
    .eye_stop("Participant identifiers collide after BIDS sanitization: ", paste(original_participants, collapse = ", "))
  }
  utils::write.table(participants, file.path(path, "participants.tsv"), sep = "\t", row.names = FALSE, quote = FALSE, na = "n/a")

  written <- list(); k <- 0L
  run_index <- ave(
    seq_len(nrow(x$recordings)),
    as.character(x$recordings$participant_id),
    FUN = seq_along
  )
  for (i in seq_len(nrow(x$recordings))) {
    rec <- x$recordings[i, , drop = FALSE]
    rec_id <- as.character(rec$recording_id[[1L]])
    participant <- paste0("sub-", .ep_sanitize_id(rec$participant_id[[1L]], "unknown"))
    run_label <- sprintf("%02d", run_index[[i]])
    beh <- file.path(path, participant, "beh")
    dir.create(beh, recursive = TRUE, showWarnings = FALSE)
    gaze <- x$gaze_samples[x$gaze_samples$recording_id == rec_id, , drop = FALSE]
    eyes <- unique(stats::na.omit(as.character(x$eye_samples$eye[x$eye_samples$recording_id == rec_id])))
    if (!length(eyes)) eyes <- "cyclopean"
    coord <- .ep_coordinate_metadata(x, rec_id)
    rate <- .ep_sampling_rate(x, rec_id)
    if (!is.finite(rate)) rate <- rec$nominal_sampling_rate[[1L]]
    if (!is.finite(rate)) rate <- 1

    for (j in seq_along(eyes)) {
      eye <- eyes[[j]]
      eye_label <- paste0("eye", j)
      eye_data <- x$eye_samples[x$eye_samples$recording_id == rec_id & x$eye_samples$eye == eye, , drop = FALSE]
      cols <- gaze[c("timestamp_native", "timestamp_seconds", "gaze_x", "gaze_y")]
      names(cols) <- c("timestamp", ".timestamp_seconds", "x_coordinate", "y_coordinate")
      if (nrow(eye_data)) {
        pupil_index <- match(cols$.timestamp_seconds, eye_data$timestamp_seconds)
        if (all(is.na(pupil_index))) pupil_index <- match(as.character(cols$timestamp), as.character(eye_data$timestamp_native))
        cols$pupil_size <- eye_data$pupil_diameter[pupil_index]
      }
      cols$.timestamp_seconds <- NULL
      cols <- cols[c("timestamp", "x_coordinate", "y_coordinate", intersect("pupil_size", names(cols)))]
      base <- paste0(
        participant, "_task-", task, "_run-", run_label,
        "_recording-", eye_label, "_physio"
      )
      tsv <- file.path(beh, paste0(base, ".tsv.gz"))
      js <- file.path(beh, paste0(base, ".json"))
      .ep_write_tsv_gz_no_header(cols, tsv)
      sidecar <- list(
        Columns = names(cols),
        PhysioType = "eyetrack",
        StartTime = 0,
        RecordedEye = switch(
          tolower(eye),
          l = "left", left = "left", os = "left",
          r = "right", right = "right", od = "right",
          both = "cyclopean", binocular = "cyclopean",
          cyclopean = "cyclopean", "cyclopean"
        ),
        SampleCoordinateSystem = coord$system,
        SamplingFrequency = rate,
        Manufacturer = as.character(rec$vendor[[1L]]),
        ManufacturersModelName = as.character(rec$device_model[[1L]]),
        SoftwareVersions = as.character(rec$software_version[[1L]]),
        timestamp = list(Description = "Native timestamp issued by the eye tracker", Units = .ep_timestamp_unit(x, rec_id)),
        x_coordinate = list(LongName = "Gaze position (x)", Units = coord$x_unit),
        y_coordinate = list(LongName = "Gaze position (y)", Units = coord$y_unit)
      )
      if ("pupil_size" %in% names(cols)) sidecar$pupil_size <- list(
        Description = "Recorded pupil diameter or area as declared by the source",
        Units = .ep_first_text(eye_data$pupil_unit, "unknown")
      )
      .ep_write_json(sidecar, js)
      k <- k + 1L
      written[[k]] <- data.frame(recording_id = rec_id, participant_id = participant, eye = eye, tsv = tsv, json = js)
    }

    events <- x$events[x$events$recording_id == rec_id, , drop = FALSE]
    if (nrow(events)) {
      ev <- data.frame(
        onset = events$timestamp_seconds,
        duration = ifelse(is.finite(events$duration), events$duration, 0),
        trial_type = events$event_type,
        value = events$event_value,
        stringsAsFactors = FALSE
      )
      ev_base <- paste0(participant, "_task-", task, "_run-", run_label, "_events")
      utils::write.table(ev, file.path(beh, paste0(ev_base, ".tsv")), sep = "\t", row.names = FALSE, quote = FALSE, na = "n/a")
      event_meta <- list(
        TaskName = task,
        onset = list(Description = "Event onset in seconds"),
        duration = list(Description = "Event duration in seconds"),
        trial_type = list(Description = "Event type")
      )
      if (!is.null(screen_distance_m) || !is.null(screen_size_m)) {
        event_meta$StimulusPresentation <- list(
          ScreenDistance = screen_distance_m,
          ScreenOrigin = c("top", "left"),
          ScreenResolution = c(rec$screen_width_px[[1L]], rec$screen_height_px[[1L]]),
          ScreenSize = screen_size_m
        )
      }
      .ep_write_json(event_meta, file.path(beh, paste0(ev_base, ".json")))
    }
  }
  manifest <- if (length(written)) do.call(rbind, written) else data.frame()
  utils::write.csv(manifest, file.path(path, "eyeprocess-bids-manifest.csv"), row.names = FALSE)
  manifest
}

#' Import Eye-Tracking-BIDS physiological recordings
#'
#' @param path BIDS dataset root.
#' @param validate Whether to validate the resulting canonical dataset.
#' @return An `eye_dataset`.
#' @export
import_eye_bids <- function(path, validate = TRUE) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  files <- list.files(path, pattern = "_recording-[A-Za-z0-9]+_physio\\.tsv\\.gz$", recursive = TRUE, full.names = TRUE)
  if (!length(files)) .eye_stop("No Eye-Tracking-BIDS physiological files were found.")
  recordings <- streams <- gaze_rows <- eye_rows <- list()
  for (i in seq_along(files)) {
    f <- files[[i]]
    js <- sub("\\.tsv\\.gz$", ".json", f)
    if (!file.exists(js)) .eye_stop("Missing BIDS sidecar: ", js)
    meta <- .ep_read_json(js)
    required_meta <- c("Columns", "PhysioType", "StartTime", "RecordedEye", "SamplingFrequency")
    missing_meta <- setdiff(required_meta, names(meta))
    if (length(missing_meta)) .eye_stop("BIDS sidecar is missing required fields: ", paste(missing_meta, collapse = ", "))
    if (!identical(tolower(as.character(meta$PhysioType)), "eyetrack")) .eye_stop("BIDS PhysioType must be `eyetrack`: ", js)
    recorded_eye <- tolower(as.character(meta$RecordedEye))
    if (!recorded_eye %in% c("left", "right", "cyclopean")) .eye_stop("Unsupported BIDS RecordedEye value: ", recorded_eye)
    if (!is.finite(as.numeric(meta$StartTime))) .eye_stop("BIDS StartTime must be finite: ", js)
    if (!is.finite(as.numeric(meta$SamplingFrequency)) || as.numeric(meta$SamplingFrequency) <= 0) {
      .eye_stop("BIDS SamplingFrequency must be positive: ", js)
    }
    columns <- as.character(meta$Columns)
    if (!all(c("timestamp", "x_coordinate", "y_coordinate") %in% columns)) {
      .eye_stop("BIDS Columns must include timestamp, x_coordinate, and y_coordinate: ", js)
    }
    con <- gzfile(f, "rt")
    d <- tryCatch(
      utils::read.table(
        con, sep = "\t", header = FALSE, stringsAsFactors = FALSE,
        na.strings = c("n/a", "NA"), quote = "", comment.char = ""
      ),
      finally = close(con)
    )
    if (ncol(d) != length(columns)) {
      .eye_stop("BIDS sidecar Columns length does not match data columns: ", f)
    }
    names(d) <- columns
    rel <- gsub("\\\\", "/", substring(f, nchar(path) + 2L))
    participant <- sub("/.*", "", rel)
    participant_id <- sub("^sub-", "", participant)
    rec_label <- sub(".*_recording-([^_]+)_physio\\.tsv\\.gz$", "\\1", basename(f))
    acquisition_label <- sub("_recording-[^_]+_physio\\.tsv\\.gz$", "", basename(f))
    rec_id <- paste0("rec_", .ep_sanitize_id(acquisition_label, participant_id))
    stream_id <- paste0(rec_id, "_", rec_label)
    recordings[[rec_id]] <- data.frame(
      recording_id = rec_id, participant_id = participant_id, session_id = NA_character_,
      vendor = as.character(meta$Manufacturer %||% "BIDS"), vendor_family = "bids",
      device_model = as.character(meta$ManufacturersModelName %||% NA_character_),
      software_name = "BIDS", software_version = as.character(meta$SoftwareVersions %||% NA_character_),
      nominal_sampling_rate = as.numeric(meta$SamplingFrequency %||% NA_real_),
      source_file_set = f, stringsAsFactors = FALSE
    )
    streams[[stream_id]] <- data.frame(
      stream_id = stream_id, recording_id = rec_id, stream_type = "gaze_eye",
      source_device = as.character(meta$ManufacturersModelName %||% NA_character_),
      source_clock = "bids_timestamp", sampling_type = "regular",
      nominal_rate_hz = as.numeric(meta$SamplingFrequency %||% NA_real_),
      observed_rate_hz = as.numeric(meta$SamplingFrequency %||% NA_real_),
      timestamp_unit = as.character(meta$timestamp$Units %||% "unknown"),
      value_unit = NA_character_, coordinate_space_id = "coord_bids", processing_level = "raw",
      stringsAsFactors = FALSE
    )
    n <- nrow(d)
    .assert_columns(d, c("timestamp", "x_coordinate", "y_coordinate"))
    timestamp_seconds <- .ep_timestamp_to_seconds(
      d$timestamp, meta$timestamp$Units %||% "unknown", as.numeric(meta$SamplingFrequency %||% NA_real_)
    ) + as.numeric(meta$StartTime %||% 0)
    gaze_rows[[i]] <- data.frame(
      recording_id = rec_id, stream_id = stream_id,
      sample_id = paste0("source_", i, "_", seq_len(n)),
      timestamp_native = d$timestamp,
      timestamp_seconds = timestamp_seconds,
      gaze_x = as.numeric(d$x_coordinate), gaze_y = as.numeric(d$y_coordinate),
      valid = is.finite(as.numeric(d$x_coordinate)) & is.finite(as.numeric(d$y_coordinate)),
      confidence = NA_real_, coordinate_space_id = "coord_bids", stringsAsFactors = FALSE
    )
    if ("pupil_size" %in% names(d)) {
      eye_rows[[i]] <- data.frame(
        recording_id = rec_id, sample_id = gaze_rows[[i]]$sample_id,
        timestamp_native = d$timestamp, timestamp_seconds = gaze_rows[[i]]$timestamp_seconds,
        eye = as.character(meta$RecordedEye %||% "cyclopean"),
        pupil_diameter = as.numeric(d$pupil_size), pupil_unit = as.character(meta$pupil_size$Units %||% "unknown"),
        pupil_valid = is.finite(as.numeric(d$pupil_size)), detector_method = "BIDS import",
        stringsAsFactors = FALSE
      )
    }
  }
  event_files <- list.files(path, pattern = "_events\\.tsv$", recursive = TRUE, full.names = TRUE)
  event_rows <- list()
  if (length(event_files)) {
    for (i in seq_along(event_files)) {
      f <- event_files[[i]]
      d <- utils::read.delim(f, stringsAsFactors = FALSE, na.strings = c("n/a", "NA"), check.names = FALSE)
      .assert_columns(d, c("onset", "duration", "trial_type"))
      rel <- gsub("\\\\", "/", substring(f, nchar(path) + 2L))
      participant <- sub("/.*", "", rel)
      participant_id <- sub("^sub-", "", participant)
      acquisition_label <- sub("_events\\.tsv$", "", basename(f))
      rec_id <- paste0("rec_", .ep_sanitize_id(acquisition_label, participant_id))
      if (!rec_id %in% names(recordings)) next
      n <- nrow(d)
      values <- if ("value" %in% names(d)) as.character(d$value) else rep(NA_character_, n)
      event_rows[[i]] <- data.frame(
        event_id = paste0(rec_id, "_bids_event_", seq_len(n)),
        recording_id = rec_id,
        timestamp_native = as.numeric(d$onset),
        timestamp_seconds = as.numeric(d$onset),
        event_type = as.character(d$trial_type),
        event_name = as.character(d$trial_type),
        event_value = values,
        duration = as.numeric(d$duration),
        source = "BIDS events.tsv",
        native_record = NA_character_,
        trial_id = values,
        stimulus_id = NA_character_,
        stringsAsFactors = FALSE
      )
    }
  }

  gaze_all <- do.call(rbind, gaze_rows)
  gaze_key <- paste(
    gaze_all$recording_id, gaze_all$timestamp_native,
    gaze_all$gaze_x, gaze_all$gaze_y, sep = "\r"
  )
  source_sample_id <- gaze_all$sample_id
  canonical_sample_id <- paste0("bids_sample_", match(gaze_key, unique(gaze_key)))
  sample_map <- stats::setNames(canonical_sample_id, source_sample_id)
  gaze_all$sample_id <- canonical_sample_id
  gaze_all <- gaze_all[!duplicated(gaze_key), , drop = FALSE]

  eye_non_null <- Filter(Negate(is.null), eye_rows)
  eye_all <- if (length(eye_non_null)) do.call(rbind, eye_non_null) else NULL
  if (!is.null(eye_all)) {
    eye_all$sample_id <- unname(sample_map[as.character(eye_all$sample_id)])
    if (anyNA(eye_all$sample_id)) .eye_stop("Could not map BIDS pupil samples to canonical gaze samples.")
  }

  coord <- new_coordinate_space("coord_bids", space_type = "custom", origin = "BIDS", x_unit = "declared", y_unit = "declared")
  out <- new_eye_dataset(
    recordings = do.call(rbind, unname(recordings)), streams = do.call(rbind, unname(streams)),
    gaze_samples = gaze_all, eye_samples = eye_all,
    events = if (length(Filter(Negate(is.null), event_rows))) do.call(rbind, Filter(Negate(is.null), event_rows)) else NULL,
    coordinate_spaces = coord, vendor_metadata = list(bids_root = path), validate = validate
  )
  add_provenance(out, "import_eye_bids", "dataset", paste0("files=", length(files)), source_files = files)
}

.ep_as_external <- function(x, mapping = NULL, vendor = "external", ...) {
  d <- as.data.frame(x, stringsAsFactors = FALSE)
  if (is.null(mapping)) mapping <- infer_eye_mapping(d)
  out <- read_eye_generic(d, mapping = mapping, ...)
  if (nrow(out$recordings)) {
    out$recordings$vendor <- vendor
    out$recordings$vendor_family <- vendor
  }
  add_provenance(out, "external_adapter", "dataset", paste0("source_class=", paste(class(x), collapse = "/"), ";vendor=", vendor))
}

#' Convert common external eye-tracking objects
#' @param x External object coercible to a data frame.
#' @param mapping Optional `eye_mapping`.
#' @param ... Passed to `read_eye_generic()`.
#' @return An `eye_dataset`.
#' @name external_eye_adapters
NULL

#' @rdname external_eye_adapters
#' @export
as_eyeprocess_eyetools <- function(x, mapping = NULL, ...) .ep_as_external(x, mapping, "eyetools", ...)
#' @rdname external_eye_adapters
#' @export
as_eyeprocess_eyetrackingr <- function(x, mapping = NULL, ...) .ep_as_external(x, mapping, "eyetrackingR", ...)
#' @rdname external_eye_adapters
#' @export
as_eyeprocess_gazer <- function(x, mapping = NULL, ...) .ep_as_external(x, mapping, "gazeR", ...)
#' @rdname external_eye_adapters
#' @export
as_eyeprocess_eyeris <- function(x, mapping = NULL, ...) .ep_as_external(x, mapping, "eyeris", ...)
#' @rdname external_eye_adapters
#' @export
as_eyeprocess_pupillometryr <- function(x, mapping = NULL, ...) .ep_as_external(x, mapping, "PupillometryR", ...)

#' Convert scanpaths to process/sequence package contracts
#'
#' @param x An `eye_dataset`.
#' @param source AOI sequence source.
#' @param collapse_consecutive Whether to collapse repeated adjacent states.
#' @param create_object For `as_traminer_sequence()`, whether to return a native
#'   `TraMineR` sequence object instead of the package-neutral wide table.
#' @return A package-compatible representation.
#' @name sequence_interoperability
NULL

#' @rdname sequence_interoperability
#' @export
as_procdata_sequence <- function(x, source = c("visits", "fixations", "samples"), collapse_consecutive = TRUE) {
  source <- match.arg(source)
  seqs <- scanpath_sequence(x, source = source, collapse_consecutive = collapse_consecutive)
  if (!nrow(seqs)) return(structure(data.frame(), class = c("eye_procdata_sequence", "data.frame")))
  rows <- lapply(seq_len(nrow(seqs)), function(i) {
    states <- strsplit(seqs$sequence[[i]], " > ", fixed = TRUE)[[1L]]
    data.frame(
      recording_id = seqs$recording_id[[i]], trial_id = seqs$trial_id[[i]],
      action_index = seq_along(states), action = states, timestamp_order = seq_along(states),
      stringsAsFactors = FALSE
    )
  })
  structure(do.call(rbind, rows), class = c("eye_procdata_sequence", "data.frame"))
}

#' @rdname sequence_interoperability
#' @export
as_traminer_sequence <- function(x, source = c("visits", "fixations", "samples"), collapse_consecutive = TRUE, create_object = FALSE) {
  source <- match.arg(source)
  seqs <- scanpath_sequence(x, source = source, collapse_consecutive = collapse_consecutive)
  split_states <- lapply(seqs$sequence, function(s) strsplit(s, " > ", fixed = TRUE)[[1L]])
  width <- if (length(split_states)) max(lengths(split_states)) else 0L
  wide <- matrix(NA_character_, nrow = length(split_states), ncol = width)
  for (i in seq_along(split_states)) wide[i, seq_along(split_states[[i]])] <- split_states[[i]]
  wide <- data.frame(recording_id = seqs$recording_id, trial_id = seqs$trial_id, wide, check.names = FALSE, stringsAsFactors = FALSE)
  names(wide)[-(1:2)] <- paste0("state_", seq_len(width))
  if (!isTRUE(create_object)) return(structure(wide, class = c("eye_traminer_sequence", "data.frame")))
  .require_namespace("TraMineR", "to create a TraMineR sequence object")
  TraMineR::seqdef(wide[-c(1, 2)], right = "DEL")
}

#' @rdname sequence_interoperability
#' @export
as_seqhmm_data <- function(x, source = c("visits", "fixations", "samples"), collapse_consecutive = TRUE) {
  source <- match.arg(source)
  seqs <- scanpath_sequence(x, source = source, collapse_consecutive = collapse_consecutive)
  states <- lapply(seqs$sequence, function(s) strsplit(s, " > ", fixed = TRUE)[[1L]])
  structure(
    list(
      sequences = states,
      lengths = lengths(states),
      alphabet = sort(unique(unlist(states, use.names = FALSE))),
      index = seqs[c("recording_id", "trial_id")]
    ),
    class = "eye_seqhmm_data"
  )
}

#' Fit a GDINA cognitive-diagnosis adapter
#' @param x An `eye_dataset`.
#' @param q_matrix Q-matrix with items in response-matrix order.
#' @param model GDINA model specification.
#' @param ... Passed to `GDINA::GDINA()`.
#' @return An `eyeprocess_model`.
#' @export
fit_gdina_adapter <- function(x, q_matrix, model = "GDINA", ...) {
  .assert_eye_dataset(x)
  y <- response_matrix(x)
  q_matrix <- as.matrix(q_matrix)
  if (nrow(q_matrix) != ncol(y)) .eye_stop("The Q-matrix must contain one row per response-matrix item.")
  .require_namespace("GDINA", "for cognitive-diagnosis models")
  fit <- GDINA::GDINA(dat = y, Q = q_matrix, model = model, ...)
  .new_eyeprocess_model(fit, "GDINA", "cognitive_diagnosis", as.data.frame(y), match.call(), list(q_matrix = q_matrix, model = model), experimental = TRUE)
}

#' Fit a diffusion IRT adapter
#' @param x An `eye_dataset`.
#' @param model Diffusion IRT model, `"D"` or `"Q"`.
#' @param ... Passed to `diffIRT::diffIRT()`.
#' @return An `eyeprocess_model`.
#' @export
fit_diffirt_adapter <- function(x, model = c("D", "Q"), ...) {
  .assert_eye_dataset(x)
  model <- match.arg(model)
  .require_namespace("diffIRT", "for diffusion IRT models")
  aligned <- align_response_matrices(response_matrix(x), response_time_matrix(x))
  fit <- diffIRT::diffIRT(rt = aligned$RT, x = aligned$Y, model = model, ...)
  .new_eyeprocess_model(fit, "diffIRT", "diffusion_irt", data.frame(), match.call(), list(model = model), experimental = TRUE)
}

#' Fit a user-defined OpenMx process model
#' @param x An `eye_dataset`.
#' @param model_builder Function receiving model data and returning an OpenMx model.
#' @param include_features Whether to merge process features.
#' @param ... Passed to `OpenMx::mxRun()`.
#' @return An `eyeprocess_model`.
#' @export
fit_openmx_process_model <- function(x, model_builder, include_features = TRUE, ...) {
  .assert_eye_dataset(x)
  if (!is.function(model_builder)) .eye_stop("`model_builder` must be a function.")
  .require_namespace("OpenMx", "for OpenMx latent-variable models")
  d <- model_data(x, include_features = include_features)
  mx <- model_builder(d)
  if (!inherits(mx, "MxModel")) .eye_stop("`model_builder` did not return an OpenMx `MxModel`.")
  fit <- OpenMx::mxRun(mx, ...)
  .new_eyeprocess_model(fit, "OpenMx", "openmx_process", d, match.call(), list(), experimental = TRUE)
}
