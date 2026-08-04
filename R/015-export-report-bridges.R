.eye_canonical_na_token <- "__EYEPROCESS_MISSING_6E7A4D2F__"
.eye_serialization_file <- ".eyeprocess-serialization.rds"

write_eye_dataset <- function(
    x,
    path,
    format = NULL,
    include_raw = FALSE,
    overwrite = FALSE,
    manifest = TRUE) {
  .assert_eye_dataset(x)
  if (is.null(format)) format <- if (tolower(tools::file_ext(path)) == "rds") "rds" else "folder"
  format <- match.arg(format, c("folder", "rds"))
  if (format == "rds") {
    if (file.exists(path) && !overwrite) .eye_stop("File exists; use `overwrite = TRUE`.")
    saveRDS(if (include_raw) x else compact_eye_dataset(x, drop_raw = TRUE), path)
    return(invisible(normalizePath(path, winslash = "/", mustWork = FALSE)))
  }
  if (dir.exists(path) && length(list.files(path, all.files = TRUE, no.. = TRUE)) && !overwrite) {
    .eye_stop("Output directory is not empty; use `overwrite = TRUE`.")
  }
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  if (isTRUE(overwrite)) {
    managed_files <- c(
      paste0(canonical_table_names(), ".csv"),
      "vendor_metadata.rds", "raw.rds", "manifest.json", "manifest.R",
      .eye_serialization_file
    )
    unlink(file.path(path, managed_files), recursive = TRUE, force = TRUE)
  }
  old_options <- options(digits = 17L)
  on.exit(options(old_options), add = TRUE)
  serialization <- list(
    format = "eyeprocess-canonical-folder",
    format_version = 1L,
    package_version = tryCatch(
      as.character(utils::packageVersion("eyeprocess")),
      error = function(e) NA_character_
    ),
    schema_version = as.character(x$schema_version),
    na_token = .eye_canonical_na_token,
    created = .now_utc()
  )
  saveRDS(serialization, file.path(path, .eye_serialization_file))
  for (nm in canonical_table_names()) {
    d <- x[[nm]]
    if ("polygon" %in% names(d) && is.list(d$polygon)) {
      d$polygon <- vapply(d$polygon, function(p) {
        if (is.null(p)) return(NA_character_)
        paste(apply(p, 1L, paste, collapse = ","), collapse = ";")
      }, character(1))
    }
    utils::write.csv(
      d,
      file.path(path, paste0(nm, ".csv")),
      row.names = FALSE,
      na = .eye_canonical_na_token
    )
  }
  saveRDS(x$vendor_metadata, file.path(path, "vendor_metadata.rds"))
  if (include_raw) saveRDS(x$raw, file.path(path, "raw.rds"))
  if (manifest) {
    man <- provenance_manifest(x)
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      jsonlite::write_json(man, file.path(path, "manifest.json"), pretty = TRUE, auto_unbox = TRUE, na = "null")
    } else {
      dput(man, file = file.path(path, "manifest.R"))
    }
  }
  invisible(normalizePath(path, winslash = "/", mustWork = FALSE))
}

read_eye_dataset <- function(path, validate = TRUE) {
  if (file.exists(path) && tolower(tools::file_ext(path)) == "rds") {
    x <- readRDS(path)
    .assert_eye_dataset(x)
    if (validate) attr(x, "validation") <- validate_eye_dataset(x)
    return(x)
  }
  if (!dir.exists(path)) .eye_stop("Canonical dataset folder does not exist: ", path)
  serialization_path <- file.path(path, .eye_serialization_file)
  serialization <- if (file.exists(serialization_path)) {
    tryCatch(readRDS(serialization_path), error = function(e) NULL)
  } else {
    NULL
  }
  na_strings <- if (is.list(serialization) && .usable_text(serialization$na_token)) {
    as.character(serialization$na_token)
  } else {
    # Compatibility with canonical folders written before format version 1.
    ""
  }
  tables <- lapply(canonical_table_names(), function(nm) {
    f <- file.path(path, paste0(nm, ".csv"))
    if (!file.exists(f)) return(empty_eye_table(nm))
    d <- utils::read.csv(
      f,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      na.strings = na_strings,
      strip.white = FALSE
    )
    if (nm == "aoi_geometry" && "polygon" %in% names(d)) {
      d$polygon <- I(lapply(d$polygon, function(s) {
        if (is.na(s) || !nzchar(s)) return(NULL)
        rows <- strsplit(s, ";", fixed = TRUE)[[1L]]
        t(vapply(rows, function(r) as.numeric(strsplit(r, ",", fixed = TRUE)[[1L]]), numeric(2)))
      }))
    }
    standardize_eye_table(d, nm)
  })
  names(tables) <- canonical_table_names()
  metadata <- if (file.exists(file.path(path, "vendor_metadata.rds"))) readRDS(file.path(path, "vendor_metadata.rds")) else list()
  raw <- if (file.exists(file.path(path, "raw.rds"))) readRDS(file.path(path, "raw.rds")) else list()
  do.call(new_eye_dataset, c(tables, list(raw = raw, vendor_metadata = metadata, validate = validate)))
}

export_canonical <- function(...) write_eye_dataset(...)
import_canonical <- function(...) read_eye_dataset(...)

write_provenance <- function(x, path, format = c("csv", "json", "rds")) {
  .assert_eye_dataset(x)
  format <- match.arg(format)
  manifest <- provenance_manifest(x)
  if (format == "csv") utils::write.csv(x$provenance, path, row.names = FALSE)
  if (format == "rds") saveRDS(manifest, path)
  if (format == "json") {
    .require_namespace("jsonlite", "to write JSON provenance")
    jsonlite::write_json(manifest, path, pretty = TRUE, auto_unbox = TRUE, na = "null")
  }
  invisible(path)
}

report_eye_dataset <- function(
    x,
    path = "eyeprocess-report.md",
    title = "eyeprocess data and analysis report",
    include_plots = FALSE,
    plot_directory = NULL) {
  .assert_eye_dataset(x)
  readiness <- analysis_readiness(x)
  validation <- validate_eye_dataset(x)
  sampling <- audit_sampling_rate(x)
  quality <- audit_signal_quality(x)
  trials <- audit_trial_coverage(x)
  warnings <- interpretive_warnings()
  lines <- c(
    paste0("# ", title), "",
    paste0("Generated: ", .now_utc()), "",
    "## Dataset", "",
    paste0("- Schema version: `", x$schema_version, "`"),
    paste0("- Recordings: ", nrow(x$recordings)),
    paste0("- Participants: ", length(unique(stats::na.omit(x$recordings$participant_id)))),
    paste0("- Gaze samples: ", nrow(x$gaze_samples)),
    paste0("- Eye samples: ", nrow(x$eye_samples)),
    paste0("- Ocular episodes: ", nrow(x$episodes)),
    paste0("- Trials: ", sum(x$intervals$interval_type == "trial")),
    paste0("- Responses: ", nrow(x$responses)),
    paste0("- Biometric observations: ", nrow(x$biometrics)),
    paste0("- Derived features: ", nrow(x$features)), "",
    "## Readiness", "",
    .markdown_table(readiness), "",
    "## Validation", "",
    if (nrow(validation)) .markdown_table(validation) else "No validation issues detected.", "",
    "## Sampling-rate audit", "",
    if (nrow(sampling)) .markdown_table(sampling) else "No gaze sampling-rate data available.", "",
    "## Signal quality", "",
    if (nrow(quality)) .markdown_table(quality) else "No signal-quality data available.", "",
    "## Trial coverage", "",
    if (nrow(trials)) .markdown_table(trials) else "No trials available.", "",
    "## Responsible interpretation", "",
    .markdown_table(warnings), "",
    "## Provenance", "",
    if (nrow(x$provenance)) .markdown_table(x$provenance) else "No provenance records available."
  )
  writeLines(lines, path, useBytes = TRUE)
  if (include_plots) {
    plot_directory <- plot_directory %||% paste0(tools::file_path_sans_ext(path), "-figures")
    dir.create(plot_directory, recursive = TRUE, showWarnings = FALSE)
    .save_plot(file.path(plot_directory, "overview.png"), function() plot_eye_overview(x))
    .save_plot(file.path(plot_directory, "signal-quality.png"), function() plot_signal_quality(x))
    .save_plot(file.path(plot_directory, "sampling-rate.png"), function() plot_sampling_rate(x))
    if (nrow(x$eye_samples)) .save_plot(file.path(plot_directory, "pupil.png"), function() plot_pupil_timeseries(x))
  }
  invisible(normalizePath(path, winslash = "/", mustWork = FALSE))
}

report_processirt <- function(...) report_eye_dataset(...)

.markdown_table <- function(d, max_rows = 50L) {
  if (!is.data.frame(d) || !nrow(d)) return("")
  d <- head(d, max_rows)
  d[] <- lapply(d, function(z) {
    z <- as.character(z)
    z[is.na(z)] <- ""
    gsub("\\|", "\\\\|", z)
  })
  header <- paste0("| ", paste(names(d), collapse = " | "), " |")
  rule <- paste0("| ", paste(rep("---", ncol(d)), collapse = " | "), " |")
  rows <- apply(d, 1L, function(z) paste0("| ", paste(z, collapse = " | "), " |"))
  paste(c(header, rule, rows), collapse = "\n")
}

.save_plot <- function(path, fun, width = 1200, height = 800, res = 120) {
  grDevices::png(path, width = width, height = height, res = res)
  on.exit(grDevices::dev.off(), add = TRUE)
  fun()
  invisible(path)
}

# Bridges -----------------------------------------------------------------

as_eye_dataset.gp3_recording <- function(x, ...) {
  if (is.data.frame(x)) {
    return(read_eye_generic(x, mapping = .gp_mapping(x), vendor = "Gazepoint", ...))
  }
  .bridge_list_to_eye_dataset(x, vendor = "Gazepoint", ...)
}

as_eye_dataset.gp3_analysis <- function(x, ...) .bridge_list_to_eye_dataset(x, vendor = "Gazepoint", ...)
as_eye_dataset.gp3_data <- function(x, ...) .bridge_list_to_eye_dataset(x, vendor = "Gazepoint", ...)

as_eye_dataset.gpbiometrics_data <- function(x, ...) {
  if (is.data.frame(x)) {
    tmp <- tempfile(fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    utils::write.csv(x, tmp, row.names = FALSE)
    return(read_gazepoint_biometrics(tmp, ...))
  }
  .bridge_list_to_eye_dataset(x, vendor = "Gazepoint Biometrics", ...)
}

.bridge_list_to_eye_dataset <- function(x, vendor = "unknown", ...) {
  if (!is.list(x)) .eye_stop("Bridge expects a list-like package object.")
  known <- intersect(names(x), canonical_table_names())
  if (length(known)) {
    args <- lapply(canonical_table_names(), function(nm) x[[nm]])
    names(args) <- canonical_table_names()
    return(do.call(new_eye_dataset, c(args, list(raw = list(source_object = x), vendor_metadata = list(bridge_vendor = vendor)))))
  }
  candidate <- NULL
  for (nm in c("data", "samples", "gaze", "raw", "recording")) {
    if (is.data.frame(x[[nm]])) { candidate <- x[[nm]]; break }
  }
  if (is.null(candidate)) .eye_stop("Could not identify a data-frame component in the source object.")
  read_eye_generic(candidate, vendor = vendor, ...)
}

as_eye_biometrics <- function(x, ...) UseMethod("as_eye_biometrics")

as_eye_biometrics.eye_dataset <- function(x, ...) {
  .assert_eye_dataset(x)
  x$biometrics
}

as_eye_biometrics.data.frame <- function(x, mapping, time_unit = "seconds", ...) {
  if (missing(mapping)) .eye_stop("A biometric channel mapping is required.")
  dummy <- x
  if (!".eye_x" %in% names(dummy)) dummy$.eye_x <- NA_real_
  if (!".eye_y" %in% names(dummy)) dummy$.eye_y <- NA_real_
  mapping$x <- mapping$x %||% ".eye_x"; mapping$y <- mapping$y %||% ".eye_y"
  read_eye_generic(dummy, mapping = mapping, time_unit = time_unit, ...)$biometrics
}
