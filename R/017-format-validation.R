# Empirical source-format validation --------------------------------------

format_validation_spec <- function(
    min_detection_confidence = 0.55,
    require_gaze = TRUE,
    require_native_time = TRUE,
    require_coordinate_space = TRUE,
    require_provenance = TRUE,
    require_raw_retention = FALSE,
    run_roundtrip = TRUE,
    numeric_tolerance = 1e-8,
    strict = FALSE) {
  if (!is.numeric(min_detection_confidence) || length(min_detection_confidence) != 1L ||
      !is.finite(min_detection_confidence) || min_detection_confidence < 0 ||
      min_detection_confidence > 1) {
    .eye_stop("`min_detection_confidence` must be between zero and one.")
  }
  if (!is.numeric(numeric_tolerance) || length(numeric_tolerance) != 1L ||
      !is.finite(numeric_tolerance) || numeric_tolerance < 0) {
    .eye_stop("`numeric_tolerance` must be a non-negative number.")
  }
  flags <- c(
    require_gaze = require_gaze,
    require_native_time = require_native_time,
    require_coordinate_space = require_coordinate_space,
    require_provenance = require_provenance,
    require_raw_retention = require_raw_retention,
    run_roundtrip = run_roundtrip,
    strict = strict
  )
  for (nm in names(flags)) .assert_flag(flags[[nm]], nm)
  structure(
    c(
      as.list(flags),
      list(
        min_detection_confidence = as.numeric(min_detection_confidence),
        numeric_tolerance = as.numeric(numeric_tolerance)
      )
    ),
    class = "eye_format_validation_spec"
  )
}

print.eye_format_validation_spec <- function(x, ...) {
  cat("<eye_format_validation_spec>\n")
  cat("  Minimum detection confidence: ", x$min_detection_confidence, "\n", sep = "")
  cat("  Require gaze:                 ", x$require_gaze, "\n", sep = "")
  cat("  Require native time:          ", x$require_native_time, "\n", sep = "")
  cat("  Require coordinate space:     ", x$require_coordinate_space, "\n", sep = "")
  cat("  Require provenance:           ", x$require_provenance, "\n", sep = "")
  cat("  Require raw retention:        ", x$require_raw_retention, "\n", sep = "")
  cat("  Run canonical round-trip:     ", x$run_roundtrip, "\n", sep = "")
  cat("  Strict validation:            ", x$strict, "\n", sep = "")
  invisible(x)
}

eye_format_profiles <- function() {
  data.frame(
    format_id = c(
      "gazepoint_analysis", "gazepoint_fixations", "gazepoint_aoi",
      "gazepoint_biometrics", "tobii_pro_lab", "pupillabs_neon",
      "pupillabs_core", "eyelink_asc", "eyelink_data_viewer",
      "eyelink_edf", "smi_begaze_text", "generic_delimited"
    ),
    vendor = c(
      "Gazepoint", "Gazepoint", "Gazepoint", "Gazepoint Biometrics",
      "Tobii", "Pupil Labs", "Pupil Labs", "SR Research",
      "SR Research", "SR Research", "SMI", "Generic"
    ),
    adapter = c(
      "gazepoint", "gazepoint", "gazepoint", "gazepoint", "tobii",
      "pupillabs", "pupillabs", "eyelink", "eyelink", "eyelink",
      "smi", "generic"
    ),
    export_family = c(
      "Analysis sample export", "Analysis fixation export", "AOI statistics",
      "Combined or paired biometrics", "Pro Lab tabular export", "Neon folder export",
      "Core Player folder export", "ASC event stream", "Data Viewer report",
      "EDF binary via EDF2ASC", "BeGaze textual export", "Mapped CSV/TSV/text"
    ),
    input_kind = c(
      "file_or_folder", "file", "file", "file_or_folder", "file",
      "folder_or_gaze_file", "folder_or_gaze_file", "file", "file",
      "binary_file", "file", "file"
    ),
    dedicated_adapter = c(
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE
    ),
    gaze = c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    pupil = c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    episodes = c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
    events = c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    aoi = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    biometrics = c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE),
    calibration = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE),
    default_time_unit = c(
      "seconds", "seconds", "seconds", "seconds", "microseconds",
      "nanoseconds", "seconds", "milliseconds", "vendor_defined",
      "milliseconds_after_conversion", "vendor_defined", "user_declared"
    ),
    default_coordinate_space = c(
      "display_normalized_top_left", "display_normalized_top_left", "display_normalized_top_left",
      "display_normalized_top_left", "display_pixels_top_left", "world_camera_pixels",
      "surface_normalized_bottom_left", "display_pixels_top_left", "vendor_defined",
      "display_pixels_top_left_after_conversion", "vendor_defined", "user_declared"
    ),
    validation_level = c(
      "synthetic_fixture", "synthetic_fixture", "declared", "synthetic_fixture",
      "synthetic_fixture", "synthetic_fixture", "synthetic_fixture",
      "synthetic_fixture", "declared", "declared", "synthetic_fixture",
      "generic_mapping"
    ),
    notes = c(
      "First-class adapter; native columns and out-of-range normalized coordinates are retained.",
      "Vendor-derived fixations remain labelled as vendor-derived.",
      "AOI summaries are retained separately from sample-level AOI assignment.",
      "Supports pupil, EDA/GSR, heart-rate, IBI, and engagement-style channels when identifiable.",
      "Handles heterogeneous gaze/event rows and separate eye validity.",
      "Uses gaze.csv plus optional fixation, event, and 3D eye-state companions.",
      "Uses gaze_positions.csv plus optional pupil and fixation companions.",
      "Parses line-oriented sample, message, fixation, saccade, and blink records.",
      "Imports delimited reports; available fields depend on the report configuration.",
      "Requires a locally available EDF2ASC converter; the package does not decode EDF directly.",
      "Supports textual BeGaze exports; proprietary IDF is not decoded directly.",
      "Requires an explicit or inferred mapping and declared units."
    ),
    stringsAsFactors = FALSE
  )
}

format_compatibility_matrix <- function(validation = NULL) {
  out <- eye_format_profiles()
  out$empirical_cases <- 0L
  out$empirical_passes <- 0L
  out$empirical_warnings <- 0L
  out$empirical_failures <- 0L
  if (inherits(validation, "eye_corpus_validation")) {
    s <- validation$summary
    normalize_key <- function(x) tolower(gsub("[^a-z0-9]+", "", as.character(x)))
    if (nrow(s)) {
      format_keys <- normalize_key(out$format_id)
      family_keys <- normalize_key(out$export_family)
      adapter_keys <- normalize_key(out$adapter)
      for (j in seq_len(nrow(s))) {
        target <- NA_integer_
        if ("format_family" %in% names(s) && .usable_text(s$format_family[j])) {
          key <- normalize_key(s$format_family[j])
          hit <- which(format_keys == key | family_keys == key)
          if (length(hit)) target <- hit[1L]
        }
        if (is.na(target)) {
          key <- normalize_key(s$vendor[j])
          hit <- which(adapter_keys == key)
          if (length(hit)) target <- hit[1L]
        }
        if (is.na(target)) next
        out$empirical_cases[target] <- out$empirical_cases[target] + 1L
        if (s$status[j] == "pass") out$empirical_passes[target] <- out$empirical_passes[target] + 1L
        if (s$status[j] == "warning") out$empirical_warnings[target] <- out$empirical_warnings[target] + 1L
        if (s$status[j] == "fail") out$empirical_failures[target] <- out$empirical_failures[target] + 1L
      }
    }
  }
  out
}

.source_delimiter <- function(path) {
  line <- tryCatch(readLines(path, n = 1L, warn = FALSE), error = function(e) character())
  if (!length(line)) return(NA_character_)
  candidates <- c(tab = "\t", comma = ",", semicolon = ";", pipe = "|")
  counts <- vapply(candidates, function(s) {
    m <- gregexpr(s, line, fixed = TRUE)[[1L]]
    if (identical(m[1L], -1L)) 0L else length(m)
  }, integer(1))
  if (!any(counts > 0L)) return(NA_character_)
  unname(candidates[which.max(counts)])
}

.source_relative_path <- function(paths, root) {
  paths <- normalizePath(paths, winslash = "/", mustWork = FALSE)
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  if (!dir.exists(root)) return(basename(paths))
  prefix <- paste0(sub("/+$", "", root), "/")
  ifelse(startsWith(paths, prefix), substring(paths, nchar(prefix) + 1L), basename(paths))
}

inspect_eye_source <- function(
    path,
    recursive = TRUE,
    inspect_rows = 10L,
    include_hash = TRUE) {
  if (!file.exists(path)) .eye_stop("Path does not exist: ", path)
  .assert_flag(recursive, "recursive")
  .assert_flag(include_hash, "include_hash")
  inspect_rows <- as.integer(inspect_rows)
  if (length(inspect_rows) != 1L || is.na(inspect_rows) || inspect_rows < 1L) {
    .eye_stop("`inspect_rows` must be a positive integer.")
  }
  root <- if (dir.exists(path)) path else dirname(path)
  files <- if (dir.exists(path)) {
    list.files(path, recursive = recursive, full.names = TRUE, all.files = FALSE)
  } else {
    path
  }
  files <- files[file.exists(files) & !dir.exists(files)]
  if (!length(files)) return(data.frame())
  rows <- lapply(files, function(f) {
    ext <- tolower(tools::file_ext(f))
    tabular <- ext %in% c("csv", "tsv", "txt", "asc")
    sample <- if (tabular) {
      tryCatch(.read_delimited(f, nrows = inspect_rows), error = function(e) NULL)
    } else {
      NULL
    }
    detection <- tryCatch(detect_eye_format(f), error = function(e) NULL)
    best_format <- if (!is.null(detection) && nrow(detection)) detection$format[1L] else NA_character_
    best_confidence <- if (!is.null(detection) && nrow(detection)) detection$confidence[1L] else NA_real_
    info <- file.info(f)
    data.frame(
      source_path = normalizePath(f, winslash = "/", mustWork = FALSE),
      relative_path = .source_relative_path(f, root),
      file_name = basename(f),
      extension = ext,
      size_bytes = as.numeric(info$size),
      modified = format(info$mtime, tz = "UTC", usetz = TRUE),
      md5 = if (include_hash) .file_md5(f) else NA_character_,
      tabular = tabular,
      readable = if (tabular) !is.null(sample) else TRUE,
      delimiter = if (tabular) .source_delimiter(f) else NA_character_,
      inspected_rows = if (is.null(sample)) 0L else nrow(sample),
      n_columns = if (is.null(sample)) NA_integer_ else ncol(sample),
      columns = if (is.null(sample)) NA_character_ else paste(names(sample), collapse = "|"),
      detected_format = best_format,
      detection_confidence = best_confidence,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(.bind_rows_base, rows)
  rownames(out) <- NULL
  out
}

.schema_critical_fields <- function() {
  list(
    recordings = c("recording_id", "vendor"),
    streams = c("stream_id", "recording_id", "stream_type", "timestamp_unit"),
    gaze_samples = c(
      "recording_id", "stream_id", "sample_id", "timestamp_native",
      "timestamp_seconds", "gaze_x", "gaze_y", "coordinate_space_id"
    ),
    eye_samples = c(
      "recording_id", "sample_id", "timestamp_native", "timestamp_seconds",
      "eye", "pupil_diameter", "pupil_unit"
    ),
    episodes = c("episode_id", "recording_id", "episode_type", "start_time", "end_time"),
    events = c("event_id", "recording_id", "timestamp_seconds", "event_type", "event_name"),
    intervals = c("interval_id", "recording_id", "interval_type", "start_time", "end_time"),
    responses = c("response_id", "recording_id", "trial_id", "item_id", "response"),
    coordinate_spaces = c("coordinate_space_id", "space_type", "origin", "x_unit", "y_unit"),
    biometrics = c("recording_id", "stream_id", "timestamp_seconds", "channel", "value", "unit"),
    provenance = c("provenance_id", "timestamp", "action", "component")
  )
}

.nonmissing_value <- function(x) {
  if (is.list(x)) return(vapply(x, function(z) !is.null(z) && length(z) > 0L, logical(1)))
  if (inherits(x, "POSIXt")) return(!is.na(x))
  if (is.character(x) || is.factor(x)) {
    z <- as.character(x)
    return(!is.na(z) & nzchar(trimws(z)))
  }
  !is.na(x)
}

schema_coverage <- function(x, require_gaze = TRUE) {
  .assert_eye_dataset(x)
  .assert_flag(require_gaze, "require_gaze")
  critical <- .schema_critical_fields()
  rows <- list()
  k <- 0L
  for (table in canonical_table_names()) {
    d <- x[[table]]
    canonical <- schema_table(table)
    for (field in canonical) {
      k <- k + 1L
      present <- field %in% names(d)
      populated <- if (present && nrow(d)) sum(.nonmissing_value(d[[field]])) else 0L
      fraction <- if (present && nrow(d)) populated / nrow(d) else NA_real_
      is_critical <- field %in% (critical[[table]] %||% character())
      status <- if (!present) {
        if (is_critical) "fail" else "warning"
      } else if (!nrow(d)) {
        if ((table == "recordings" || (table == "gaze_samples" && require_gaze)) && is_critical) "fail" else "not_applicable"
      } else if (is_critical && populated == 0L) {
        "fail"
      } else if (is_critical && fraction < 1) {
        "warning"
      } else if (populated == 0L) {
        "empty"
      } else {
        "pass"
      }
      rows[[k]] <- data.frame(
        table = table,
        field = field,
        rows = nrow(d),
        present = present,
        populated_n = populated,
        populated_fraction = fraction,
        critical = is_critical,
        status = status,
        stringsAsFactors = FALSE
      )
    }
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

schema_coverage_summary <- function(x) {
  coverage <- if (inherits(x, "eye_dataset")) schema_coverage(x) else x
  .assert_data_frame(coverage, "x")
  groups <- split(coverage, coverage$table)
  out <- lapply(groups, function(d) {
    data.frame(
      table = d$table[1L],
      rows = max(d$rows),
      canonical_fields = nrow(d),
      populated_fields = sum(d$populated_n > 0L),
      critical_fields = sum(d$critical),
      critical_pass = sum(d$critical & d$status == "pass"),
      failures = sum(d$status == "fail"),
      warnings = sum(d$status == "warning"),
      populated_fraction = mean(d$populated_n > 0L),
      stringsAsFactors = FALSE
    )
  })
  ans <- do.call(rbind, out)
  rownames(ans) <- NULL
  ans
}

.usable_text <- function(x) {
  z <- trimws(as.character(x))
  !is.na(z) & nzchar(z) & !z %in% c("NA", "<data.frame>", "<redacted>")
}

source_preservation_audit <- function(x, require_raw = FALSE) {
  .assert_eye_dataset(x)
  .assert_flag(require_raw, "require_raw")
  gaze <- x$gaze_samples
  eye <- x$eye_samples
  biometric <- x$biometrics
  native_values <- c(gaze$timestamp_native, eye$timestamp_native, biometric$timestamp_native)
  normalized_values <- c(gaze$timestamp_seconds, eye$timestamp_seconds, biometric$timestamp_seconds)
  pupil_units <- unique(stats::na.omit(eye$pupil_unit[is.finite(eye$pupil_diameter)]))
  checks <- list(
    c("source_provenance", nrow(x$provenance) > 0L, nrow(x$provenance), "At least one provenance row is retained."),
    c("source_file_reference", any(.usable_text(x$provenance$source_files)), sum(.usable_text(x$provenance$source_files)), "Source-file references are recorded."),
    c("source_file_hash", any(.usable_text(x$provenance$file_hashes)), sum(.usable_text(x$provenance$file_hashes)), "Source-file hashes are recorded when files are available."),
    c("native_timestamps", length(native_values) > 0L && any(is.finite(.safe_numeric(native_values))), sum(is.finite(.safe_numeric(native_values))), "Native timestamps are retained."),
    c("normalized_timestamps", length(normalized_values) > 0L && any(is.finite(.safe_numeric(normalized_values))), sum(is.finite(.safe_numeric(normalized_values))), "Seconds-based timestamps are available."),
    c("coordinate_registry", nrow(x$coordinate_spaces) > 0L, nrow(x$coordinate_spaces), "Coordinate spaces are explicit."),
    c("stream_units", nrow(x$streams) == 0L || any(nzchar(stats::na.omit(x$streams$timestamp_unit))), sum(nzchar(stats::na.omit(x$streams$timestamp_unit))), "Stream timestamp units are explicit."),
    c("pupil_units", !nrow(eye) || !any(is.finite(eye$pupil_diameter)) || length(pupil_units) > 0L, length(pupil_units), "Pupil units are explicit when pupil values exist."),
    c("vendor_metadata", length(x$vendor_metadata) > 0L, length(x$vendor_metadata), "Vendor-specific metadata are retained."),
    c("raw_retention", !require_raw || length(x$raw) > 0L, length(x$raw), if (require_raw) "Raw source data are required for this validation." else "Raw retention is optional for this validation.")
  )
  out <- do.call(rbind, lapply(checks, function(z) {
    ok <- as.logical(z[[2L]])
    data.frame(
      check = as.character(z[[1L]]),
      status = if (isTRUE(ok)) "pass" else "fail",
      value = suppressWarnings(as.numeric(z[[3L]])),
      message = as.character(z[[4L]]),
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  out
}

.stable_cell <- function(x) {
  if (is.list(x)) {
    return(vapply(x, function(z) {
      if (is.null(z) || !length(z)) return("<NA>")
      paste(capture.output(dput(z)), collapse = "")
    }, character(1)))
  }
  if (inherits(x, "POSIXt")) return(format(x, tz = "UTC", usetz = TRUE))
  if (is.numeric(x)) {
    out <- rep("<NA>", length(x))
    finite <- is.finite(x)
    out[finite] <- format(x[finite], digits = 17L, scientific = FALSE, trim = TRUE)
    out[is.infinite(x) & x > 0] <- "Inf"
    out[is.infinite(x) & x < 0] <- "-Inf"
    return(out)
  }
  if (is.logical(x)) {
    out <- as.character(x)
    out[is.na(x)] <- "<NA>"
    return(out)
  }
  out <- as.character(x)
  out[is.na(out)] <- "<NA>"
  out
}

.stable_table <- function(d, ignore_columns = character()) {
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  keep <- setdiff(names(d), ignore_columns)
  d <- d[keep]
  if (!ncol(d)) return(data.frame())
  out <- as.data.frame(lapply(d, .stable_cell), stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(out)) {
    key <- do.call(paste, c(out, sep = "\r"))
    out <- out[order(key, method = "radix"), , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}

.table_md5 <- function(d) {
  f <- tempfile(fileext = ".txt")
  on.exit(unlink(f), add = TRUE)
  utils::write.table(d, f, sep = "\t", quote = TRUE, row.names = FALSE, col.names = TRUE, na = "<NA>", fileEncoding = "UTF-8")
  unname(tools::md5sum(f))
}

fingerprint_eye_dataset <- function(
    x,
    tables = canonical_table_names(),
    ignore_volatile = TRUE) {
  .assert_eye_dataset(x)
  tables <- intersect(as.character(tables), canonical_table_names())
  volatile <- list(
    provenance = c("provenance_id", "timestamp"),
    quality = c("quality_id", "computed_at"),
    features = c("feature_id", "derived_at")
  )
  out <- lapply(tables, function(table) {
    ignore <- if (isTRUE(ignore_volatile)) volatile[[table]] %||% character() else character()
    stable <- .stable_table(x[[table]], ignore_columns = ignore)
    data.frame(
      table = table,
      rows = nrow(x[[table]]),
      columns = ncol(x[[table]]),
      compared_columns = ncol(stable),
      md5 = .table_md5(stable),
      stringsAsFactors = FALSE
    )
  })
  ans <- do.call(rbind, out)
  rownames(ans) <- NULL
  ans
}

compare_eye_datasets <- function(
    x,
    y,
    tables = canonical_table_names(),
    numeric_tolerance = 1e-8,
    ignore_volatile = TRUE) {
  .assert_eye_dataset(x)
  .assert_eye_dataset(y)
  if (!is.numeric(numeric_tolerance) || length(numeric_tolerance) != 1L ||
      !is.finite(numeric_tolerance) || numeric_tolerance < 0) {
    .eye_stop("`numeric_tolerance` must be a non-negative number.")
  }
  tables <- intersect(as.character(tables), canonical_table_names())
  volatile <- list(
    provenance = c("provenance_id", "timestamp"),
    quality = c("quality_id", "computed_at"),
    features = c("feature_id", "derived_at")
  )
  key_fields <- list(
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
    aoi_geometry = c("aoi_id", "valid_from", "valid_to", "frame_id"),
    biometrics = c("recording_id", "stream_id", "timestamp_native", "channel", "trial_id"),
    calibrations = "calibration_id",
    features = "feature_id",
    quality = "quality_id",
    provenance = "provenance_id"
  )
  order_rows <- function(d, table, common) {
    keys <- intersect(key_fields[[table]] %||% character(), common)
    if (length(keys) && nrow(d)) {
      stable_keys <- .stable_table(d[keys])
      key <- do.call(paste, c(stable_keys, sep = "\r"))
      if (!anyDuplicated(key)) return(order(key, method = "radix"))
    }
    if (!nrow(d)) return(integer())
    stable <- .stable_table(d[common])
    key <- if (ncol(stable)) do.call(paste, c(stable, sep = "\r")) else rep("", nrow(d))
    order(key, method = "radix")
  }
  compare_column <- function(a, b) {
    if (length(a) != length(b)) return(c(differences = max(length(a), length(b)), max_numeric_difference = Inf))
    if (is.list(a) || is.list(b)) {
      aa <- .stable_cell(a)
      bb <- .stable_cell(b)
      return(c(differences = sum(aa != bb), max_numeric_difference = NA_real_))
    }
    na_a <- is.na(a)
    na_b <- is.na(b)
    differences <- sum(na_a != na_b)
    keep <- !na_a & !na_b
    if (!any(keep)) return(c(differences = differences, max_numeric_difference = 0))
    if (is.numeric(a) && is.numeric(b)) {
      aa <- as.numeric(a[keep])
      bb <- as.numeric(b[keep])
      same_inf <- is.infinite(aa) & is.infinite(bb) & sign(aa) == sign(bb)
      finite <- is.finite(aa) & is.finite(bb)
      delta <- rep(Inf, length(aa))
      delta[same_inf] <- 0
      delta[finite] <- abs(aa[finite] - bb[finite])
      differences <- differences + sum(delta > numeric_tolerance)
      max_delta <- if (length(delta)) max(delta) else 0
      return(c(differences = differences, max_numeric_difference = max_delta))
    }
    aa <- as.character(a[keep])
    bb <- as.character(b[keep])
    c(differences = differences + sum(aa != bb), max_numeric_difference = NA_real_)
  }
  out <- lapply(tables, function(table) {
    dx <- as.data.frame(x[[table]], stringsAsFactors = FALSE)
    dy <- as.data.frame(y[[table]], stringsAsFactors = FALSE)
    missing_in_y <- setdiff(names(dx), names(dy))
    added_in_y <- setdiff(names(dy), names(dx))
    common <- intersect(names(dx), names(dy))
    ignore <- if (isTRUE(ignore_volatile)) volatile[[table]] %||% character() else character()
    common <- setdiff(common, ignore)
    if (nrow(dx) == nrow(dy) && nrow(dx)) {
      dx <- dx[order_rows(dx, table, common), , drop = FALSE]
      dy <- dy[order_rows(dy, table, common), , drop = FALSE]
    }
    comparisons <- lapply(common, function(field) compare_column(dx[[field]], dy[[field]]))
    differing_cells <- if (length(comparisons)) sum(vapply(comparisons, `[[`, numeric(1), "differences")) else 0
    numeric_differences <- if (length(comparisons)) vapply(comparisons, `[[`, numeric(1), "max_numeric_difference") else numeric()
    numeric_differences <- numeric_differences[is.finite(numeric_differences)]
    max_numeric_difference <- if (length(numeric_differences)) max(numeric_differences) else NA_real_
    status <- if (
      nrow(dx) == nrow(dy) && !length(missing_in_y) && !length(added_in_y) && differing_cells == 0
    ) "pass" else "fail"
    data.frame(
      table = table,
      rows_original = nrow(dx),
      rows_restored = nrow(dy),
      columns_original = ncol(dx),
      columns_restored = ncol(dy),
      missing_columns = paste(missing_in_y, collapse = "|"),
      added_columns = paste(added_in_y, collapse = "|"),
      differing_cells = differing_cells,
      max_numeric_difference = max_numeric_difference,
      content_equal = differing_cells == 0,
      status = status,
      stringsAsFactors = FALSE
    )
  })
  ans <- do.call(rbind, out)
  rownames(ans) <- NULL
  ans
}

roundtrip_eye_dataset <- function(
    x,
    path = tempfile("eyeprocess-roundtrip-"),
    include_raw = FALSE,
    numeric_tolerance = 1e-8,
    cleanup = TRUE) {
  .assert_eye_dataset(x)
  .assert_flag(include_raw, "include_raw")
  .assert_flag(cleanup, "cleanup")
  if (cleanup) on.exit(unlink(path, recursive = TRUE, force = TRUE), add = TRUE)
  write_eye_dataset(x, path, format = "folder", include_raw = include_raw, overwrite = TRUE)
  restored <- read_eye_dataset(path, validate = TRUE)
  comparison <- compare_eye_datasets(x, restored, numeric_tolerance = numeric_tolerance)
  out <- list(
    status = if (all(comparison$status == "pass")) "pass" else "fail",
    comparison = comparison,
    original_fingerprint = fingerprint_eye_dataset(x),
    restored_fingerprint = fingerprint_eye_dataset(restored),
    path = normalizePath(path, winslash = "/", mustWork = FALSE),
    numeric_tolerance = numeric_tolerance
  )
  class(out) <- "eye_roundtrip_validation"
  out
}

print.eye_roundtrip_validation <- function(x, ...) {
  cat("eyeprocess canonical round-trip: ", toupper(x$status), "\n", sep = "")
  print.data.frame(x$comparison, row.names = FALSE)
  invisible(x)
}

.validation_check <- function(check, status, value = NA_real_, message = NA_character_) {
  data.frame(
    check = as.character(check),
    status = as.character(status),
    value = as.numeric(value),
    message = as.character(message),
    stringsAsFactors = FALSE
  )
}

.safe_audit <- function(fun, x) {
  tryCatch(fun(x), error = function(e) data.frame(
    severity = "warning", code = "audit_failed", table = NA_character_,
    field = NA_character_, message = conditionMessage(e), stringsAsFactors = FALSE
  ))
}

validate_eye_source <- function(
    path,
    vendor = "auto",
    spec = format_validation_spec(),
    import_args = list(),
    retain_dataset = FALSE,
    case_id = NULL) {
  if (!inherits(spec, "eye_format_validation_spec")) {
    .eye_stop("`spec` must be created by `format_validation_spec()`.")
  }
  if (!is.list(import_args)) .eye_stop("`import_args` must be a list.")
  .assert_flag(retain_dataset, "retain_dataset")
  if (!file.exists(path)) .eye_stop("Path does not exist: ", path)
  case_id <- case_id %||% basename(normalizePath(path, winslash = "/", mustWork = FALSE))
  started <- .now_utc()
  source <- inspect_eye_source(path)
  detection <- tryCatch(detect_eye_format(path), error = function(e) {
    structure(data.frame(
      format = character(), confidence = numeric(), priority = numeric(),
      stringsAsFactors = FALSE
    ), class = c("eye_format_detection", "data.frame"))
  })
  selected_vendor <- vendor
  if (identical(vendor, "auto") && nrow(detection)) selected_vendor <- detection$format[1L]
  selected_row <- if (nrow(detection)) match(selected_vendor, detection$format) else NA_integer_
  confidence <- if (!is.na(selected_row)) detection$confidence[selected_row] else 0
  adapter_issues <- .empty_vendor_issues()
  adapter <- .eye_env$adapters[[selected_vendor]]
  if (!is.null(adapter) && is.function(adapter$validate)) {
    adapter_issues <- tryCatch(adapter$validate(path), error = function(e) data.frame(
      severity = "error", code = "adapter_validation_error", file = as.character(path),
      message = conditionMessage(e), stringsAsFactors = FALSE
    ))
  }
  imported <- tryCatch(
    do.call(read_eye_export, c(list(path = path, vendor = selected_vendor), import_args)),
    error = function(e) e
  )
  import_error <- if (inherits(imported, "error")) conditionMessage(imported) else NA_character_
  dataset <- if (is_eye_dataset(imported)) imported else NULL
  validation <- if (!is.null(dataset)) validate_eye_dataset(dataset, strict = spec$strict) else data.frame()
  coverage <- if (!is.null(dataset)) schema_coverage(dataset, require_gaze = spec$require_gaze) else data.frame()
  preservation <- if (!is.null(dataset)) source_preservation_audit(dataset, require_raw = spec$require_raw_retention) else data.frame()
  audits <- if (!is.null(dataset)) list(
    timebase = .safe_audit(audit_timebase, dataset),
    coordinates = .safe_audit(audit_coordinate_spaces, dataset),
    sampling_rate = .safe_audit(audit_sampling_rate, dataset),
    signal_quality = .safe_audit(audit_signal_quality, dataset),
    event_order = .safe_audit(audit_event_order, dataset)
  ) else list()
  roundtrip <- if (!is.null(dataset) && isTRUE(spec$run_roundtrip)) {
    tryCatch(roundtrip_eye_dataset(dataset, numeric_tolerance = spec$numeric_tolerance), error = function(e) structure(
      list(
        status = "fail",
        comparison = data.frame(
          table = NA_character_, rows_original = NA_integer_, rows_restored = NA_integer_,
          columns_original = NA_integer_, columns_restored = NA_integer_,
          missing_columns = NA_character_, added_columns = NA_character_,
          content_equal = FALSE, status = "fail", stringsAsFactors = FALSE
        ),
        error = conditionMessage(e)
      ),
      class = "eye_roundtrip_validation"
    ))
  } else NULL
  detection_status <- if (confidence >= spec$min_detection_confidence) {
    "pass"
  } else if (!identical(vendor, "auto") && selected_vendor %in% names(.eye_env$adapters)) {
    "warning"
  } else {
    "fail"
  }
  checks <- list(
    .validation_check(
      "format_detection",
      detection_status,
      confidence,
      paste0("Selected adapter: ", selected_vendor, ".")
    ),
    .validation_check(
      "import",
      if (!is.null(dataset)) "pass" else "fail",
      if (!is.null(dataset)) 1 else 0,
      if (!is.null(dataset)) "Source imported to an eye_dataset." else import_error
    )
  )
  if (!is.null(dataset)) {
    checks[[length(checks) + 1L]] <- .validation_check(
      "dataset_validation",
      if (any(validation$severity == "error")) "fail" else if (nrow(validation)) "warning" else "pass",
      sum(validation$severity == "error"),
      paste0(nrow(validation), " validation issue(s).")
    )
    critical_coverage <- coverage[coverage$critical, , drop = FALSE]
    checks[[length(checks) + 1L]] <- .validation_check(
      "critical_schema_coverage",
      if (any(critical_coverage$status == "fail")) "fail" else if (any(critical_coverage$status == "warning")) "warning" else "pass",
      mean(critical_coverage$status == "pass"),
      "Critical canonical fields were assessed."
    )
    gaze_ok <- nrow(dataset$gaze_samples) > 0L
    checks[[length(checks) + 1L]] <- .validation_check(
      "gaze_samples",
      if (!spec$require_gaze || gaze_ok) "pass" else "fail",
      nrow(dataset$gaze_samples),
      if (spec$require_gaze) "Gaze observations are required." else "Gaze observations are optional."
    )
    native_ok <- any(preservation$check == "native_timestamps" & preservation$status == "pass")
    checks[[length(checks) + 1L]] <- .validation_check(
      "native_time_preservation",
      if (!spec$require_native_time || native_ok) "pass" else "fail",
      if (native_ok) 1 else 0,
      "Native and normalized time should remain distinguishable."
    )
    coord_ok <- any(preservation$check == "coordinate_registry" & preservation$status == "pass")
    checks[[length(checks) + 1L]] <- .validation_check(
      "coordinate_space",
      if (!spec$require_coordinate_space || coord_ok) "pass" else "fail",
      nrow(dataset$coordinate_spaces),
      "Coordinate semantics should be explicit."
    )
    prov_ok <- any(preservation$check == "source_provenance" & preservation$status == "pass")
    checks[[length(checks) + 1L]] <- .validation_check(
      "provenance",
      if (!spec$require_provenance || prov_ok) "pass" else "fail",
      nrow(dataset$provenance),
      "Transformations and source references should be auditable."
    )
    raw_ok <- length(dataset$raw) > 0L
    checks[[length(checks) + 1L]] <- .validation_check(
      "raw_retention",
      if (!spec$require_raw_retention || raw_ok) "pass" else "fail",
      length(dataset$raw),
      if (spec$require_raw_retention) "Raw source retention is required." else "Raw source retention is optional."
    )
    if (!is.null(roundtrip)) {
      checks[[length(checks) + 1L]] <- .validation_check(
        "canonical_roundtrip",
        roundtrip$status,
        mean(roundtrip$comparison$status == "pass"),
        "Canonical folder export and re-import were compared table by table."
      )
    }
  }
  checks <- do.call(rbind, checks)
  adapter_fail <- nrow(adapter_issues) && any(adapter_issues$severity == "error")
  if (adapter_fail) {
    checks <- rbind(checks, .validation_check(
      "adapter_specific_validation", "fail",
      sum(adapter_issues$severity == "error"),
      "The selected adapter reported source-format errors."
    ))
  } else if (nrow(adapter_issues)) {
    checks <- rbind(checks, .validation_check(
      "adapter_specific_validation",
      if (any(adapter_issues$severity == "warning")) "warning" else "pass",
      nrow(adapter_issues),
      "The selected adapter completed its source-format checks."
    ))
  }
  overall <- if (any(checks$status == "fail")) "fail" else if (any(checks$status == "warning") ||
      (nrow(adapter_issues) && any(adapter_issues$severity == "warning"))) "warning" else "pass"
  out <- list(
    case_id = as.character(case_id),
    path = normalizePath(path, winslash = "/", mustWork = FALSE),
    vendor = as.character(selected_vendor),
    status = overall,
    started = started,
    completed = .now_utc(),
    spec = spec,
    source = source,
    detection = detection,
    adapter_issues = adapter_issues,
    checks = checks,
    validation = validation,
    coverage = coverage,
    preservation = preservation,
    audits = audits,
    roundtrip = roundtrip,
    import_error = import_error,
    dataset = if (retain_dataset) dataset else NULL
  )
  class(out) <- "eye_format_validation"
  out
}

print.eye_format_validation <- function(x, ...) {
  cat("eyeprocess source-format validation\n")
  cat("Case:    ", x$case_id, "\n", sep = "")
  cat("Vendor:  ", x$vendor, "\n", sep = "")
  cat("Status:  ", toupper(x$status), "\n", sep = "")
  cat("Path:    ", x$path, "\n", sep = "")
  print.data.frame(x$checks, row.names = FALSE)
  invisible(x)
}

summary.eye_format_validation <- function(object, ...) {
  data.frame(
    case_id = object$case_id,
    path = object$path,
    vendor = object$vendor,
    status = object$status,
    files = nrow(object$source),
    detection_confidence = if (nrow(object$detection)) object$detection$confidence[1L] else 0,
    imported = any(object$checks$check == "import" & object$checks$status == "pass"),
    validation_errors = if ("severity" %in% names(object$validation)) sum(object$validation$severity == "error") else NA_integer_,
    validation_warnings = if ("severity" %in% names(object$validation)) sum(object$validation$severity == "warning") else NA_integer_,
    roundtrip = if (is.null(object$roundtrip)) "not_run" else object$roundtrip$status,
    stringsAsFactors = FALSE
  )
}

plot.eye_format_validation <- function(x, type = c("checks", "coverage"), ...) {
  type <- match.arg(type)
  if (type == "checks") {
    d <- x$checks
    if (!nrow(d)) .eye_stop("No validation checks are available.")
    values <- c(pass = 3, warning = 2, fail = 1)
    y <- unname(values[d$status])
    graphics::barplot(
      y,
      names.arg = d$check,
      las = 2,
      ylim = c(0, 3.3),
      ylab = "Validation status",
      main = paste0("Source-format validation: ", x$case_id),
      ...
    )
    graphics::axis(2, at = 1:3, labels = c("Fail", "Warning", "Pass"), las = 1)
    return(invisible(d))
  }
  d <- schema_coverage_summary(x$coverage)
  if (!nrow(d)) .eye_stop("No schema-coverage information is available.")
  graphics::barplot(
    d$populated_fraction,
    names.arg = d$table,
    las = 2,
    ylim = c(0, 1),
    ylab = "Populated canonical fields",
    main = paste0("Canonical coverage: ", x$case_id),
    ...
  )
  invisible(d)
}

validation_manifest <- function(
    paths,
    vendor = "auto",
    format_family = NA_character_,
    software_version = NA_character_,
    device_model = NA_character_,
    expected_import = TRUE,
    require_gaze = TRUE,
    require_native_time = TRUE,
    require_coordinate_space = TRUE,
    require_provenance = TRUE,
    require_raw_retention = FALSE,
    run_roundtrip = TRUE,
    case_id = NULL,
    notes = NA_character_) {
  paths <- as.character(paths)
  if (!length(paths)) .eye_stop("At least one source path is required.")
  n <- length(paths)
  recycle <- function(x, name) {
    if (length(x) == 1L) return(rep(x, n))
    if (length(x) != n) .eye_stop("`", name, "` must have length one or match `paths`.")
    x
  }
  case_id <- if (is.null(case_id)) make.unique(basename(normalizePath(paths, winslash = "/", mustWork = FALSE))) else recycle(as.character(case_id), "case_id")
  out <- data.frame(
    case_id = case_id,
    path = normalizePath(paths, winslash = "/", mustWork = FALSE),
    vendor = recycle(as.character(vendor), "vendor"),
    format_family = recycle(as.character(format_family), "format_family"),
    software_version = recycle(as.character(software_version), "software_version"),
    device_model = recycle(as.character(device_model), "device_model"),
    expected_import = recycle(expected_import, "expected_import"),
    require_gaze = recycle(require_gaze, "require_gaze"),
    require_native_time = recycle(require_native_time, "require_native_time"),
    require_coordinate_space = recycle(require_coordinate_space, "require_coordinate_space"),
    require_provenance = recycle(require_provenance, "require_provenance"),
    require_raw_retention = recycle(require_raw_retention, "require_raw_retention"),
    run_roundtrip = recycle(run_roundtrip, "run_roundtrip"),
    notes = recycle(as.character(notes), "notes"),
    stringsAsFactors = FALSE
  )
  .validate_manifest_rows(out)
}

.coerce_manifest_flag <- function(x, field) {
  if (is.logical(x)) return(x)
  if (is.numeric(x)) {
    invalid <- !is.na(x) & !x %in% c(0, 1)
    if (any(invalid)) .eye_stop("Manifest field `", field, "` must contain TRUE, FALSE, or blank values.")
    return(ifelse(is.na(x), NA, x == 1))
  }
  z <- tolower(trimws(as.character(x)))
  out <- rep(NA, length(z))
  out[z %in% c("true", "t", "yes", "y", "1")] <- TRUE
  out[z %in% c("false", "f", "no", "n", "0")] <- FALSE
  blank <- is.na(z) | !nzchar(z) | z == "na"
  invalid <- !blank & is.na(out)
  if (any(invalid)) {
    .eye_stop(
      "Manifest field `", field, "` contains invalid logical value(s): ",
      paste(unique(z[invalid]), collapse = ", "), "."
    )
  }
  out
}

.validate_manifest_rows <- function(x, name = "validation manifest") {
  .assert_data_frame(x, name)
  .assert_columns(x, c("case_id", "path", "vendor"), name)
  ids <- trimws(as.character(x$case_id))
  if (any(is.na(ids) | !nzchar(ids))) .eye_stop("Every validation case requires a non-empty `case_id`.")
  if (anyDuplicated(ids)) {
    duplicate_ids <- unique(ids[duplicated(ids) | duplicated(ids, fromLast = TRUE)])
    .eye_stop("Validation `case_id` values must be unique: ", paste(duplicate_ids, collapse = ", "), ".")
  }
  paths <- trimws(as.character(x$path))
  if (any(is.na(paths) | !nzchar(paths))) .eye_stop("Every validation case requires a non-empty `path`.")
  vendors <- trimws(as.character(x$vendor))
  if (any(is.na(vendors) | !nzchar(vendors))) .eye_stop("Every validation case requires a non-empty `vendor`.")
  x$case_id <- ids
  x$path <- paths
  x$vendor <- vendors
  flag_fields <- intersect(c(
    "expected_import", "require_gaze", "require_native_time",
    "require_coordinate_space", "require_provenance",
    "require_raw_retention", "run_roundtrip"
  ), names(x))
  for (field in flag_fields) x[[field]] <- .coerce_manifest_flag(x[[field]], field)
  x
}

.is_absolute_path <- function(x) {
  grepl("^(?:[A-Za-z]:[/\\\\]|/|\\\\\\\\)", x, perl = TRUE)
}

write_validation_manifest <- function(x, path) {
  x <- .validate_manifest_rows(x, "x")
  utils::write.csv(x, path, row.names = FALSE, na = "")
  invisible(normalizePath(path, winslash = "/", mustWork = FALSE))
}

read_validation_manifest <- function(path) {
  if (!file.exists(path)) .eye_stop("Manifest does not exist: ", path)
  manifest_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  x <- utils::read.csv(manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
  x <- .validate_manifest_rows(x)
  relative <- !.is_absolute_path(x$path)
  if (any(relative)) {
    x$path[relative] <- file.path(dirname(manifest_path), x$path[relative])
  }
  x$path <- normalizePath(x$path, winslash = "/", mustWork = FALSE)
  x
}

init_validation_corpus <- function(path, overwrite = FALSE) {
  .assert_scalar_character(path, "path")
  .assert_flag(overwrite, "overwrite")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  manifest_path <- file.path(path, "validation-manifest.csv")
  readme_path <- file.path(path, "README.txt")
  manifest <- data.frame(
    case_id = character(), path = character(), vendor = character(),
    format_family = character(), software_version = character(),
    device_model = character(), expected_import = logical(),
    require_gaze = logical(), require_native_time = logical(),
    require_coordinate_space = logical(), require_provenance = logical(),
    require_raw_retention = logical(), run_roundtrip = logical(),
    notes = character(), stringsAsFactors = FALSE
  )
  instructions <- c(
    "eyeprocess real-export validation corpus",
    "",
    "1. Create one subdirectory or add one source file per de-identified export case.",
    "2. Add one unique row per case to validation-manifest.csv.",
    "3. Use adapter names returned by eyeprocess::supported_eye_formats().",
    "4. Prefer format_id values returned by eyeprocess::eye_format_profiles().",
    "5. Keep this corpus outside public version control.",
    "6. Run eyeprocess::validate_eye_corpus() and review every warning.",
    "7. Review anonymized bundles manually before sharing."
  )
  wrote <- character()
  if (!file.exists(manifest_path) || isTRUE(overwrite)) {
    write_validation_manifest(manifest, manifest_path)
    wrote <- c(wrote, basename(manifest_path))
  }
  if (!file.exists(readme_path) || isTRUE(overwrite)) {
    writeLines(instructions, readme_path, useBytes = TRUE)
    wrote <- c(wrote, basename(readme_path))
  }
  if (!length(wrote)) {
    .eye_message(
      "Validation corpus is already initialized; existing manifest and case files were preserved."
    )
  } else {
    .eye_message("Initialized validation corpus template: ", paste(wrote, collapse = ", "), ".")
  }
  invisible(normalizePath(path, winslash = "/", mustWork = TRUE))
}

discover_validation_cases <- function(path, recursive = FALSE) {
  if (!dir.exists(path)) .eye_stop("Validation corpus directory does not exist: ", path)
  .assert_flag(recursive, "recursive")
  children <- list.files(path, full.names = TRUE, recursive = FALSE, all.files = FALSE)
  dirs <- children[dir.exists(children)]
  files <- children[file.exists(children) & !dir.exists(children)]
  files <- files[!tolower(basename(files)) %in% c("manifest.csv", "validation-manifest.csv")]
  cases <- c(dirs, files)
  if (!length(cases) && recursive) {
    cases <- list.files(path, full.names = TRUE, recursive = TRUE)
    cases <- cases[file.exists(cases) & !dir.exists(cases)]
  }
  if (!length(cases)) {
    return(data.frame(
      case_id = character(), path = character(), vendor = character(),
      format_family = character(), software_version = character(),
      device_model = character(), expected_import = logical(),
      require_gaze = logical(), require_native_time = logical(),
      require_coordinate_space = logical(), require_provenance = logical(),
      require_raw_retention = logical(), run_roundtrip = logical(),
      notes = character(), stringsAsFactors = FALSE
    ))
  }
  validation_manifest(cases)
}

validate_eye_corpus <- function(
    manifest,
    spec = format_validation_spec(),
    import_args = list(),
    retain_datasets = FALSE,
    stop_on_failure = FALSE) {
  if (is.character(manifest) && length(manifest) == 1L) {
    if (dir.exists(manifest)) {
      manifest_file <- file.path(manifest, "validation-manifest.csv")
      manifest <- if (file.exists(manifest_file)) read_validation_manifest(manifest_file) else discover_validation_cases(manifest)
    } else {
      manifest <- read_validation_manifest(manifest)
    }
  }
  manifest <- .validate_manifest_rows(manifest, "manifest")
  if (!nrow(manifest)) .eye_stop("The validation corpus contains no cases.")
  .assert_flag(retain_datasets, "retain_datasets")
  .assert_flag(stop_on_failure, "stop_on_failure")
  results <- vector("list", nrow(manifest))
  for (i in seq_len(nrow(manifest))) {
    args <- import_args
    if (is.list(import_args) && !is.null(names(import_args)) && manifest$case_id[i] %in% names(import_args)) {
      args <- import_args[[manifest$case_id[i]]]
    }
    case_spec <- spec
    override_fields <- c(
      "require_gaze", "require_native_time", "require_coordinate_space",
      "require_provenance", "require_raw_retention", "run_roundtrip"
    )
    for (field in intersect(override_fields, names(manifest))) {
      value <- manifest[[field]][i]
      if (!is.na(value)) case_spec[[field]] <- isTRUE(value)
    }
    results[[i]] <- validate_eye_source(
      manifest$path[i],
      vendor = manifest$vendor[i],
      spec = case_spec,
      import_args = args,
      retain_dataset = retain_datasets,
      case_id = manifest$case_id[i]
    )
  }
  names(results) <- manifest$case_id
  summary <- do.call(rbind, lapply(results, summary))
  extra <- intersect(c(
    "format_family", "software_version", "device_model", "expected_import",
    "require_gaze", "require_native_time", "require_coordinate_space",
    "require_provenance", "require_raw_retention", "run_roundtrip", "notes"
  ), names(manifest))
  if (length(extra)) {
    match_index <- match(summary$case_id, manifest$case_id)
    for (field in extra) summary[[field]] <- manifest[[field]][match_index]
  }
  summary$validation_status <- summary$status
  expected <- if ("expected_import" %in% names(summary)) summary$expected_import else rep(TRUE, nrow(summary))
  expected[is.na(expected)] <- TRUE
  summary$expectation_met <- ifelse(expected, summary$imported, !summary$imported)
  summary$status <- ifelse(
    !summary$expectation_met,
    "fail",
    ifelse(expected, summary$validation_status, "pass")
  )
  out <- list(
    manifest = manifest,
    results = results,
    summary = summary,
    status = if (any(summary$status == "fail")) "fail" else if (any(summary$status == "warning")) "warning" else "pass",
    completed = .now_utc()
  )
  class(out) <- "eye_corpus_validation"
  if (stop_on_failure && out$status == "fail") {
    .eye_stop("The validation corpus contains ", sum(summary$status == "fail"), " failed case(s).")
  }
  out
}

print.eye_corpus_validation <- function(x, ...) {
  cat("eyeprocess validation corpus\n")
  cat("Cases:   ", nrow(x$summary), "\n", sep = "")
  cat("Status:  ", toupper(x$status), "\n", sep = "")
  print.data.frame(x$summary, row.names = FALSE)
  invisible(x)
}

plot.eye_corpus_validation <- function(x, type = c("status", "vendor"), ...) {
  type <- match.arg(type)
  d <- x$summary
  if (!nrow(d)) .eye_stop("The validation corpus is empty.")
  if (type == "status") {
    counts <- table(factor(d$status, levels = c("pass", "warning", "fail")))
    graphics::barplot(counts, ylab = "Cases", main = "Export-validation corpus", ...)
    return(invisible(counts))
  }
  counts <- table(d$vendor, d$status)
  graphics::barplot(counts, beside = TRUE, las = 2, ylab = "Cases", main = "Validation status by adapter", ...)
  invisible(counts)
}

.redact_metadata_paths <- function(x) {
  if (is.list(x)) {
    out <- x
    for (i in seq_along(out)) {
      nm <- names(out)[i] %||% ""
      if (grepl("(^|_)(source_)?(file|path|directory|folder)(s)?($|_)|participant|subject|user_id|email|person_name", tolower(nm))) {
        out[[i]] <- "<redacted>"
      } else {
        out[[i]] <- .redact_metadata_paths(out[[i]])
      }
    }
    return(out)
  }
  x
}

anonymize_eye_dataset <- function(
    x,
    drop_raw = TRUE,
    strip_source_paths = TRUE,
    redact_free_text = TRUE,
    anonymize_aois = TRUE,
    retain_map = FALSE,
    participant_prefix = "P",
    recording_prefix = "R",
    session_prefix = "S") {
  .assert_eye_dataset(x)
  for (nm in c("drop_raw", "strip_source_paths", "redact_free_text", "anonymize_aois", "retain_map")) {
    .assert_flag(get(nm), nm)
  }
  out <- .deep_copy(x)
  maps <- list()

  remap_field <- function(object, field, tables, prefix, linked_fields = field) {
    values <- unique(stats::na.omit(unlist(lapply(tables, function(table) {
      d <- object[[table]]
      if (!is.data.frame(d) || !field %in% names(d)) return(character())
      as.character(d[[field]])
    }), use.names = FALSE)))
    values <- values[nzchar(values)]
    map <- setNames(paste0(prefix, sprintf("%07d", seq_along(values))), values)
    if (!length(map)) return(list(object = object, map = map))
    for (table in tables) {
      d <- object[[table]]
      if (!is.data.frame(d) || !nrow(d)) next
      for (target in linked_fields) {
        if (!target %in% names(d)) next
        idx <- match(as.character(d[[target]]), names(map))
        replace <- !is.na(idx)
        d[[target]][replace] <- unname(map[idx[replace]])
      }
      object[[table]] <- d
    }
    list(object = object, map = map)
  }

  apply_map <- function(field, tables, prefix, linked_fields = field, map_name = field) {
    result <- remap_field(out, field, tables, prefix, linked_fields)
    out <<- result$object
    maps[[map_name]] <<- result$map
  }

  apply_map(
    "participant_id",
    c("recordings", "intervals", "responses", "features"),
    participant_prefix,
    map_name = "participants"
  )
  apply_map(
    "recording_id",
    canonical_table_names(),
    recording_prefix,
    map_name = "recordings"
  )
  apply_map("session_id", "recordings", session_prefix, map_name = "sessions")
  apply_map("stream_id", c("streams", "gaze_samples", "biometrics", "quality"), "ST", map_name = "streams")
  apply_map("sample_id", "gaze_samples", "GS", map_name = "gaze_samples")
  apply_map("sample_id", "eye_samples", "ES", map_name = "eye_samples")
  apply_map("episode_id", "episodes", "EP", map_name = "episodes")
  apply_map("event_id", "events", "EV", map_name = "events")
  apply_map("interval_id", "intervals", "IN", linked_fields = c("interval_id", "parent_interval_id"), map_name = "intervals")
  apply_map("response_id", "responses", "RS", map_name = "responses")
  apply_map("calibration_id", "calibrations", "CA", map_name = "calibrations")
  apply_map("feature_id", "features", "FT", map_name = "features")
  apply_map("quality_id", "quality", "QL", map_name = "quality")
  apply_map("provenance_id", "provenance", "PV", map_name = "provenance")
  apply_map(
    "trial_id",
    c("gaze_samples", "eye_samples", "episodes", "events", "intervals", "responses", "biometrics", "features", "quality"),
    "TR",
    map_name = "trials"
  )
  apply_map("item_id", c("intervals", "responses", "features"), "IT", map_name = "items")
  apply_map(
    "stimulus_id",
    c("gaze_samples", "eye_samples", "episodes", "events", "intervals", "aoi_definitions", "features", "biometrics"),
    "SM",
    map_name = "stimuli"
  )
  if (anonymize_aois) {
    apply_map(
      "aoi_id",
      c("episodes", "aoi_definitions", "aoi_geometry", "features"),
      "AO",
      linked_fields = c("aoi_id", "parent_aoi_id"),
      map_name = "aois"
    )
  }
  if (redact_free_text) {
    apply_map("condition_id", "intervals", "CO", map_name = "conditions")
    if (nrow(out$events)) {
      event_names <- unique(stats::na.omit(as.character(out$events$event_name)))
      event_names <- event_names[nzchar(event_names)]
      event_map <- setNames(paste0("event_", sprintf("%05d", seq_along(event_names))), event_names)
      idx <- match(as.character(out$events$event_name), names(event_map))
      replace <- !is.na(idx)
      out$events$event_name[replace] <- unname(event_map[idx[replace]])
      out$events$event_value <- NA_character_
      out$events$native_record <- NA_character_
      maps$event_names <- event_map
    }
    if (nrow(out$aoi_definitions)) {
      out$aoi_definitions$aoi_name <- ifelse(
        is.na(out$aoi_definitions$aoi_name),
        NA_character_,
        paste0("aoi_", sprintf("%05d", seq_len(nrow(out$aoi_definitions))))
      )
    }
    if (nrow(out$responses) && (is.character(out$responses$response) || is.factor(out$responses$response))) {
      out$responses$response <- as.character(out$responses$response)
      response_values <- unique(stats::na.omit(out$responses$response))
      response_values <- response_values[nzchar(response_values)]
      response_map <- setNames(paste0("response_", sprintf("%05d", seq_along(response_values))), response_values)
      idx <- match(out$responses$response, names(response_map))
      replace <- !is.na(idx)
      out$responses$response[replace] <- unname(response_map[idx[replace]])
      maps$response_values <- response_map
    }
    if (nrow(out$recordings)) out$recordings$experiment_type <- NA_character_
    if (nrow(out$coordinate_spaces)) out$coordinate_spaces$reference_object <- NA_character_
    if (nrow(out$provenance)) {
      out$provenance$details <- "<redacted>"
      out$provenance$warnings <- ifelse(is.na(out$provenance$warnings), NA_character_, "<redacted>")
    }
  }
  if (strip_source_paths) {
    out$recordings$source_file_set <- NA_character_
    out$provenance$source_files <- NA_character_
    out$provenance$file_hashes <- NA_character_
    out$vendor_metadata <- if (redact_free_text) {
      list(redacted = TRUE)
    } else {
      .redact_metadata_paths(out$vendor_metadata)
    }
  }
  if (drop_raw) out$raw <- list()
  out <- add_provenance(
    out,
    "anonymize_dataset",
    "dataset",
    "Linked identifiers and potentially identifying source metadata were transformed.",
    source_files = character(),
    reversible = FALSE,
    warnings = "Automated anonymization requires study-specific human review."
  )
  if (retain_map) attr(out, "anonymization_map") <- maps
  attr(out, "validation") <- validate_eye_dataset(out)
  out
}

write_format_validation_report <- function(x, path = "eyeprocess-format-validation.md") {
  if (inherits(x, "eye_format_validation")) {
    lines <- c(
      paste0("# eyeprocess source-format validation: ", x$case_id), "",
      paste0("- Status: **", toupper(x$status), "**"),
      paste0("- Adapter: `", x$vendor, "`"),
      paste0("- Source: `", x$path, "`"),
      paste0("- Completed: ", x$completed), "",
      "## Checks", "", .markdown_table(x$checks), "",
      "## Source files", "", if (nrow(x$source)) .markdown_table(x$source) else "No source files were inspected.", "",
      "## Format detection", "", if (nrow(x$detection)) .markdown_table(x$detection) else "No format was detected.", "",
      "## Adapter-specific findings", "", if (nrow(x$adapter_issues)) .markdown_table(x$adapter_issues) else "No adapter-specific findings.", "",
      "## Canonical validation", "", if (nrow(x$validation)) .markdown_table(x$validation) else "No canonical validation issues.", "",
      "## Schema coverage", "", if (nrow(x$coverage)) .markdown_table(schema_coverage_summary(x$coverage)) else "No imported dataset was available.", "",
      "## Source preservation", "", if (nrow(x$preservation)) .markdown_table(x$preservation) else "No imported dataset was available.", "",
      "## Canonical round-trip", "",
      if (is.null(x$roundtrip)) "Round-trip validation was not run." else .markdown_table(x$roundtrip$comparison)
    )
  } else if (inherits(x, "eye_corpus_validation")) {
    lines <- c(
      "# eyeprocess export-validation corpus", "",
      paste0("- Status: **", toupper(x$status), "**"),
      paste0("- Cases: ", nrow(x$summary)),
      paste0("- Completed: ", x$completed), "",
      "## Corpus summary", "", .markdown_table(x$summary), "",
      "## Compatibility matrix", "", .markdown_table(format_compatibility_matrix(x))
    )
    for (nm in names(x$results)) {
      result <- x$results[[nm]]
      lines <- c(
        lines, "", paste0("## ", nm), "",
        paste0("Status: **", toupper(result$status), "**"), "",
        .markdown_table(result$checks)
      )
    }
  } else {
    .eye_stop("`x` must be an `eye_format_validation` or `eye_corpus_validation` object.")
  }
  writeLines(lines, path, useBytes = TRUE)
  invisible(normalizePath(path, winslash = "/", mustWork = FALSE))
}

.redact_validation_result <- function(x) {
  out <- x
  sensitive <- unique(c(
    as.character(x$path),
    if (.usable_text(x$path)) dirname(as.character(x$path)) else character(),
    if (is.data.frame(x$source) && "source_path" %in% names(x$source)) as.character(x$source$source_path) else character(),
    if (is.data.frame(x$adapter_issues) && "file" %in% names(x$adapter_issues)) as.character(x$adapter_issues$file) else character()
  ))
  sensitive <- sensitive[.usable_text(sensitive)]
  scrub_text <- function(z) {
    out_text <- as.character(z)
    for (value in sensitive) out_text <- gsub(value, "<redacted>", out_text, fixed = TRUE)
    out_text
  }
  scrub_frame <- function(d) {
    if (!is.data.frame(d) || !nrow(d)) return(d)
    for (field in names(d)) {
      if (is.character(d[[field]]) || is.factor(d[[field]])) d[[field]] <- scrub_text(d[[field]])
    }
    d
  }
  out$case_id <- "case_00001"
  out$path <- "<redacted>"
  out$checks <- scrub_frame(out$checks)
  out$validation <- scrub_frame(out$validation)
  out$preservation <- scrub_frame(out$preservation)
  out$adapter_issues <- scrub_frame(out$adapter_issues)
  if (nrow(out$adapter_issues) && "file" %in% names(out$adapter_issues)) out$adapter_issues$file <- "<redacted>"
  if (is.data.frame(out$source) && nrow(out$source)) {
    ext <- if ("extension" %in% names(out$source)) as.character(out$source$extension) else rep("", nrow(out$source))
    safe_name <- paste0(
      "source_", sprintf("%05d", seq_len(nrow(out$source))),
      ifelse(nzchar(ext), paste0(".", ext), "")
    )
    if ("source_path" %in% names(out$source)) out$source$source_path <- "<redacted>"
    if ("relative_path" %in% names(out$source)) out$source$relative_path <- safe_name
    if ("file_name" %in% names(out$source)) out$source$file_name <- safe_name
    if ("md5" %in% names(out$source)) out$source$md5 <- NA_character_
    if ("modified" %in% names(out$source)) out$source$modified <- NA_character_
    if ("columns" %in% names(out$source)) out$source$columns <- scrub_text(out$source$columns)
  }
  if (!is.null(out$roundtrip) && !is.null(out$roundtrip$path)) out$roundtrip$path <- "<redacted>"
  out$import_error <- if (.usable_text(out$import_error)) scrub_text(out$import_error) else out$import_error
  out$dataset <- NULL
  out
}

create_validation_bundle <- function(
    x,
    path = "eyeprocess-validation-bundle.zip",
    include_dataset = TRUE,
    anonymize = TRUE,
    overwrite = FALSE) {
  if (!inherits(x, "eye_format_validation")) {
    .eye_stop("`x` must be an `eye_format_validation` object.")
  }
  .assert_flag(include_dataset, "include_dataset")
  .assert_flag(anonymize, "anonymize")
  .assert_flag(overwrite, "overwrite")
  if (file.exists(path) && !overwrite) .eye_stop("Output file exists; use `overwrite = TRUE`.")
  if (include_dataset && is.null(x$dataset)) {
    .eye_stop("The validation did not retain its dataset. Re-run with `retain_dataset = TRUE`.")
  }
  root <- tempfile("eyeprocess-validation-bundle-")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  evidence <- if (anonymize) .redact_validation_result(x) else x
  write_format_validation_report(evidence, file.path(root, "validation-report.md"))
  utils::write.csv(evidence$checks, file.path(root, "checks.csv"), row.names = FALSE, na = "")
  utils::write.csv(evidence$source, file.path(root, "source-manifest.csv"), row.names = FALSE, na = "")
  utils::write.csv(evidence$detection, file.path(root, "format-detection.csv"), row.names = FALSE, na = "")
  utils::write.csv(evidence$adapter_issues, file.path(root, "adapter-findings.csv"), row.names = FALSE, na = "")
  utils::write.csv(evidence$validation, file.path(root, "canonical-validation.csv"), row.names = FALSE, na = "")
  utils::write.csv(evidence$coverage, file.path(root, "schema-coverage.csv"), row.names = FALSE, na = "")
  utils::write.csv(evidence$preservation, file.path(root, "source-preservation.csv"), row.names = FALSE, na = "")
  if (!is.null(evidence$roundtrip)) {
    utils::write.csv(evidence$roundtrip$comparison, file.path(root, "roundtrip-comparison.csv"), row.names = FALSE, na = "")
  }
  if (include_dataset) {
    dataset <- if (anonymize) anonymize_eye_dataset(x$dataset) else x$dataset
    write_eye_dataset(dataset, file.path(root, "canonical-dataset"), include_raw = FALSE, overwrite = TRUE)
  }
  metadata <- c(
    "Bundle-Version: 1",
    paste0("Package-Version: ", tryCatch(as.character(utils::packageVersion("eyeprocess")), error = function(e) "development")),
    paste0("Case-ID: ", evidence$case_id),
    paste0("Adapter: ", evidence$vendor),
    paste0("Validation-Status: ", x$status),
    paste0("Created: ", .now_utc()),
    paste0("Anonymized: ", anonymize),
    "Raw-Source-Included: FALSE"
  )
  writeLines(metadata, file.path(root, "BUNDLE"), useBytes = TRUE)
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  old <- setwd(root)
  on.exit(setwd(old), add = TRUE)
  files <- list.files(".", recursive = TRUE, all.files = TRUE, no.. = TRUE)
  utils::zip(path, files = files)
  invisible(path)
}

.empty_vendor_issues <- function() {
  data.frame(
    severity = character(), code = character(), file = character(),
    message = character(), stringsAsFactors = FALSE
  )
}

.vendor_issue <- function(severity, code, file, message) {
  data.frame(
    severity = severity,
    code = code,
    file = normalizePath(file, winslash = "/", mustWork = FALSE),
    message = message,
    stringsAsFactors = FALSE
  )
}

validate_tobii_export <- function(path) {
  issues <- .empty_vendor_issues()
  if (dir.exists(path)) return(.vendor_issue("error", "expected_file", path, "Tobii Pro Lab validation expects a delimited export file."))
  d <- tryCatch(.read_delimited(path, nrows = 5L), error = function(e) NULL)
  if (is.null(d)) return(.vendor_issue("error", "unreadable_export", path, "The Tobii export could not be read as a delimited table."))
  nms <- tolower(names(d))
  has_time <- any(grepl("recording timestamp|system_time_stamp|device_time_stamp|timestamp", nms))
  has_gaze <- any(grepl("gaze point.*x|gaze2d x|display_area_x", nms)) && any(grepl("gaze point.*y|gaze2d y|display_area_y", nms))
  if (!has_time) issues <- rbind(issues, .vendor_issue("error", "missing_timestamp", path, "No supported Tobii timestamp column was identified."))
  if (!has_gaze) issues <- rbind(issues, .vendor_issue("error", "missing_gaze_coordinates", path, "No supported Tobii gaze-coordinate pair was identified."))
  if (!any(grepl("validity|valid", nms))) issues <- rbind(issues, .vendor_issue("warning", "missing_validity", path, "No explicit Tobii gaze-validity field was identified."))
  issues
}

validate_pupillabs_export <- function(path) {
  issues <- .empty_vendor_issues()
  format <- pupil_labs_format(path)
  if (format == "unknown") return(.vendor_issue("error", "unknown_pupil_format", path, "The source is not recognizable as Pupil Labs Neon or Core."))
  required <- if (format == "neon") "gaze.csv" else "gaze_positions.csv"
  if (dir.exists(path) && !file.exists(file.path(path, required))) {
    issues <- rbind(issues, .vendor_issue("error", "missing_gaze_file", path, paste0("Required `", required, "` is absent.")))
  }
  issues
}

validate_eyelink_export <- function(path) {
  if (dir.exists(path)) return(.vendor_issue("error", "expected_file", path, "EyeLink validation expects EDF, ASC, or a Data Viewer report file."))
  ext <- tolower(tools::file_ext(path))
  if (ext == "edf") {
    return(.vendor_issue("warning", "external_converter_required", path, "EDF requires a locally installed EDF2ASC converter before text parsing."))
  }
  lines <- tryCatch(readLines(path, n = 50L, warn = FALSE), error = function(e) character())
  if (!length(lines)) return(.vendor_issue("error", "unreadable_export", path, "The EyeLink source could not be read."))
  if (ext == "asc") {
    tokens <- sub("[[:space:]].*$", "", trimws(lines))
    known <- tokens %in% c("MSG", "START", "END", "EFIX", "ESACC", "EBLINK", "SFIX", "SSACC", "SBLINK", "BUTTON", "INPUT", "PRESCALER", "EVENTS", "SAMPLES") | grepl("^[0-9]+$", tokens)
    if (!any(known)) return(.vendor_issue("error", "unknown_asc_records", path, "No recognized EyeLink ASC record types were found."))
  }
  .empty_vendor_issues()
}

validate_smi_export <- function(path) {
  if (dir.exists(path)) return(.vendor_issue("error", "expected_file", path, "SMI validation expects a textual BeGaze export file."))
  if (tolower(tools::file_ext(path)) == "idf") {
    return(.vendor_issue("error", "proprietary_idf", path, "Direct IDF decoding is not supported; export text from BeGaze first."))
  }
  confidence <- is_smi_export(path)
  if (confidence < 0.5) return(.vendor_issue("warning", "low_smi_confidence", path, "The textual export weakly matches known SMI/BeGaze fields."))
  .empty_vendor_issues()
}

validate_generic_export <- function(path) {
  if (dir.exists(path)) return(.vendor_issue("error", "expected_file", path, "Generic mapping expects one delimited file."))
  d <- tryCatch(.read_delimited(path, nrows = 10L), error = function(e) NULL)
  if (is.null(d)) return(.vendor_issue("error", "unreadable_export", path, "The generic export could not be read as a delimited table."))
  mapping <- infer_eye_mapping(d)
  missing <- setdiff(c("timestamp", "x", "y"), names(mapping))
  if (length(missing)) {
    return(.vendor_issue("warning", "mapping_required", path, paste0("Explicit mappings are required for: ", paste(missing, collapse = ", "), ".")))
  }
  .empty_vendor_issues()
}
