# Batch validation of de-identified real eye-tracking exports.
# Edit these two paths before running the script.

invisible((function() {
  corpus_path <- "path/to/eyeprocess-validation-corpus"
  output_path <- "path/to/eyeprocess-validation-output"

  if (!requireNamespace("eyeprocess", quietly = TRUE)) {
    stop("Install eyeprocess before running this script.", call. = FALSE)
  }

  manifest_path <- file.path(corpus_path, "validation-manifest.csv")
  if (!file.exists(manifest_path)) {
    stop(
      "No validation manifest was found at: ", manifest_path,
      "\nRun eyeprocess::init_validation_corpus() first.",
      call. = FALSE
    )
  }

  manifest <- eyeprocess::read_validation_manifest(manifest_path)
  if (!nrow(manifest)) {
    message(
      "The validation corpus is initialized but contains no cases.\n",
      "Add one de-identified export file or folder per case, then add one row to:\n  ",
      normalizePath(manifest_path, winslash = "/", mustWork = FALSE), "\n",
      "Use eyeprocess::supported_eye_formats() for adapter names and\n",
      "eyeprocess::eye_format_profiles() for format_family values.\n",
      "No validation was run and no output files were created."
    )
    return(invisible(NULL))
  }

  missing_paths <- !file.exists(manifest$path)
  if (any(missing_paths)) {
    stop(
      "The manifest contains missing source path(s):\n - ",
      paste(manifest$path[missing_paths], collapse = "\n - "),
      call. = FALSE
    )
  }

  dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
  result <- eyeprocess::validate_eye_corpus(
    manifest,
    spec = eyeprocess::format_validation_spec(
      min_detection_confidence = 0.55,
      require_gaze = TRUE,
      require_native_time = TRUE,
      require_coordinate_space = TRUE,
      require_provenance = TRUE,
      require_raw_retention = FALSE,
      run_roundtrip = TRUE,
      strict = FALSE
    ),
    retain_datasets = FALSE,
    stop_on_failure = FALSE
  )

  saveRDS(result, file.path(output_path, "eyeprocess-validation-corpus.rds"))
  utils::write.csv(
    result$summary,
    file.path(output_path, "validation-summary.csv"),
    row.names = FALSE,
    na = ""
  )
  utils::write.csv(
    eyeprocess::format_compatibility_matrix(result),
    file.path(output_path, "compatibility-matrix.csv"),
    row.names = FALSE,
    na = ""
  )
  eyeprocess::write_format_validation_report(
    result,
    file.path(output_path, "validation-report.md")
  )

  print(result)
  cat(
    "\nValidation outputs written to:\n",
    normalizePath(output_path, winslash = "/"), "\n",
    sep = ""
  )
  invisible(result)
})())
