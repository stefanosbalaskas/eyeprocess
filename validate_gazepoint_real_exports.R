# Empirical validation for the private Gazepoint Analysis 7.2.0 corpus.

invisible((function() {
  corpus_root <- "path/to/eyeprocess-validation-corpus"
  case_path <- file.path(corpus_root, "cases", "gazepoint-analysis-v7.2.0-demo")
  output_path <- "path/to/eyeprocess-validation-output/gazepoint-v7.2.0"

  if (!requireNamespace("eyeprocess", quietly = TRUE)) {
    stop("Install eyeprocess before running this script.", call. = FALSE)
  }
  if (!dir.exists(case_path)) {
    stop(
      "The Gazepoint validation case was not found at: ", case_path,
      "\nRun prepare_gazepoint_validation_corpus.ps1 or extract the prepared corpus ZIP first.",
      call. = FALSE
    )
  }

  dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
  pairs <- eyeprocess::gp_pair_exports(case_path)
  pair_audit <- eyeprocess::gp_audit_file_pairs(case_path)
  dataset <- eyeprocess::read_gazepoint_folder(case_path, keep_raw = TRUE)
  validation <- eyeprocess::validate_eye_dataset(dataset, stop_on_error = FALSE)
  if (nrow(validation) && any(validation$severity == "error")) {
    print(validation)
    stop("Canonical Gazepoint import contains validation errors.", call. = FALSE)
  }

  evidence <- eyeprocess::validate_eye_source(
    case_path,
    vendor = "gazepoint",
    spec = eyeprocess::format_validation_spec(
      min_detection_confidence = 0.90,
      require_gaze = TRUE,
      require_native_time = TRUE,
      require_coordinate_space = TRUE,
      require_provenance = TRUE,
      require_raw_retention = TRUE,
      run_roundtrip = TRUE,
      strict = FALSE
    ),
    retain_dataset = FALSE,
    case_id = "gazepoint-analysis-v7-2-0-six-users"
  )

  if (!evidence$status %in% c("pass", "warning")) {
    print(evidence)
    stop("Real Gazepoint validation failed.", call. = FALSE)
  }

  utils::write.csv(pairs, file.path(output_path, "file-pairs.csv"), row.names = FALSE, na = "")
  utils::write.csv(pair_audit, file.path(output_path, "pair-audit.csv"), row.names = FALSE, na = "")
  utils::write.csv(dataset$recordings, file.path(output_path, "recordings.csv"), row.names = FALSE, na = "")
  utils::write.csv(dataset$streams, file.path(output_path, "streams.csv"), row.names = FALSE, na = "")
  utils::write.csv(eyeprocess::schema_coverage_summary(dataset), file.path(output_path, "schema-coverage.csv"), row.names = FALSE, na = "")
  utils::write.csv(eyeprocess::source_preservation_audit(dataset, require_raw = TRUE), file.path(output_path, "source-preservation.csv"), row.names = FALSE, na = "")
  utils::write.csv(validation, file.path(output_path, "canonical-validation.csv"), row.names = FALSE, na = "")
  eyeprocess::write_format_validation_report(evidence, file.path(output_path, "validation-report.md"))
  saveRDS(eyeprocess::compact_eye_dataset(dataset, drop_raw = TRUE), file.path(output_path, "canonical-gazepoint-dataset.rds"))
  saveRDS(evidence, file.path(output_path, "validation-evidence.rds"))

  cat("\nReal Gazepoint Analysis 7.2.0 validation completed.\n")
  cat("Status:        ", toupper(evidence$status), "\n", sep = "")
  cat("Recordings:    ", nrow(dataset$recordings), "\n", sep = "")
  cat("Gaze samples:  ", nrow(dataset$gaze_samples), "\n", sep = "")
  cat("Fixations:     ", sum(dataset$episodes$episode_type == "fixation"), "\n", sep = "")
  cat("Eye samples:   ", nrow(dataset$eye_samples), "\n", sep = "")
  cat("Biometrics:    ", nrow(dataset$biometrics), "\n", sep = "")
  cat("AOIs:          ", nrow(dataset$aoi_definitions), "\n", sep = "")
  cat("Features:      ", nrow(dataset$features), "\n", sep = "")
  cat("Outputs:       ", normalizePath(output_path, winslash = "/"), "\n", sep = "")

  invisible(list(dataset = dataset, evidence = evidence, output_path = output_path))
})())
