test_that("format profiles distinguish declared and empirical support", {
  profiles <- eye_format_profiles()
  expect_s3_class(profiles, "data.frame")
  expect_gte(nrow(profiles), 12)
  expect_true(all(c("format_id", "vendor", "adapter", "validation_level") %in% names(profiles)))
  expect_true(any(profiles$format_id == "gazepoint_analysis"))
  expect_true(all(c("gazepoint", "tobii", "pupillabs", "eyelink", "smi", "generic") %in% unique(profiles$adapter)))
})

test_that("source inspection records fields and hashes", {
  p <- extdata("gazepoint", "demo-user.csv")
  out <- inspect_eye_source(p)
  expect_equal(nrow(out), 1)
  expect_equal(out$file_name, "demo-user.csv")
  expect_true(out$readable)
  expect_gt(out$n_columns, 5)
  expect_true(nzchar(out$md5))
})

test_that("schema coverage and source preservation are structured", {
  x <- read_gazepoint(extdata("gazepoint", "demo-user.csv"), recording_id = "R1", quiet = TRUE)
  coverage <- schema_coverage(x)
  expect_true(all(c("table", "field", "populated_fraction", "critical", "status") %in% names(coverage)))
  expect_true(any(coverage$table == "gaze_samples" & coverage$field == "timestamp_native" & coverage$status == "pass"))
  summary <- schema_coverage_summary(coverage)
  expect_true(any(summary$table == "gaze_samples"))
  preservation <- source_preservation_audit(x)
  expect_true(all(c("check", "status", "value", "message") %in% names(preservation)))
  expect_true(any(preservation$check == "native_timestamps" & preservation$status == "pass"))
})

test_that("canonical round-trip is loss-aware", {
  x <- simulate_eye_dataset(
    n_person = 2,
    n_item = 2,
    sampling_rate = 20,
    trial_duration = 0.25,
    seed = 101
  )
  expect_warning(result <- roundtrip_eye_dataset(x), NA)
  expect_s3_class(result, "eye_roundtrip_validation")
  expect_equal(result$status, "pass")
  expect_true(all(result$comparison$status == "pass"))
})



test_that("binocular eye samples use a composite primary key", {
  x <- read_gazepoint(
    extdata("gazepoint", "demo-user.csv"),
    recording_id = "R1",
    quiet = TRUE
  )
  issues <- validate_eye_dataset(x)
  duplicate_eye_key <- issues$code == "duplicate_primary_key" & issues$table == "eye_samples"
  expect_false(any(duplicate_eye_key))
})

test_that("canonical folder serialization distinguishes missing and empty text", {
  x <- simulate_eye_dataset(
    n_person = 2,
    n_item = 2,
    sampling_rate = 20,
    trial_duration = 0.25,
    seed = 102
  )
  x$recordings$firmware_version[1] <- NA_character_
  x$recordings$experiment_type[1] <- ""
  folder <- tempfile("eyeprocess-serialization-")
  on.exit(unlink(folder, recursive = TRUE, force = TRUE), add = TRUE)
  write_eye_dataset(x, folder, format = "folder", overwrite = TRUE)
  restored <- read_eye_dataset(folder)
  expect_true(is.na(restored$recordings$firmware_version[1]))
  expect_identical(restored$recordings$experiment_type[1], "")
  comparison <- compare_eye_datasets(x, restored)
  expect_true(all(comparison$status == "pass"))
})

test_that("single-source validation produces reproducible evidence", {
  spec <- format_validation_spec(run_roundtrip = FALSE)
  result <- validate_eye_source(
    extdata("gazepoint", "demo-user.csv"),
    vendor = "gazepoint",
    spec = spec,
    import_args = list(recording_id = "R1", quiet = TRUE),
    retain_dataset = TRUE,
    case_id = "gp-demo"
  )
  expect_s3_class(result, "eye_format_validation")
  expect_true(result$status %in% c("pass", "warning"))
  expect_true(any(result$checks$check == "import" & result$checks$status == "pass"))
  expect_s3_class(result$dataset, "eye_dataset")
  expect_true(all(!is.na(result$dataset$biometrics$unit)))
  expect_true(all(nzchar(result$dataset$biometrics$unit)))
  expect_s3_class(summary(result), "data.frame")
})

test_that("anonymization removes source identifiers without breaking keys", {
  x <- read_gazepoint(extdata("gazepoint", "demo-user.csv"), recording_id = "recording-secret", quiet = TRUE)
  y <- anonymize_eye_dataset(x)
  expect_s3_class(y, "eye_dataset")
  expect_true(all(grepl("^R[0-9]{7}$", y$recordings$recording_id)))
  expect_true(all(grepl("^P[0-9]{7}$", y$recordings$participant_id)))
  all_values <- unlist(lapply(
    y[canonical_table_names()],
    function(d) unlist(lapply(d, as.character), use.names = FALSE)
  ), use.names = FALSE)
  expect_false(any(grepl("recording-secret", all_values, fixed = TRUE)))
  expect_length(y$raw, 0)
  expect_null(attr(y, "anonymization_map"))
  expect_false(any(validate_eye_dataset(y)$severity == "error"))
})

test_that("validation corpora aggregate vendor cases", {
  manifest <- validation_manifest(
    c(
      extdata("gazepoint", "demo-user.csv"),
      extdata("tobii-demo.tsv")
    ),
    vendor = c("gazepoint", "tobii"),
    case_id = c("gp", "tobii")
  )
  result <- validate_eye_corpus(
    manifest,
    spec = format_validation_spec(run_roundtrip = FALSE),
    import_args = list(
      gp = list(recording_id = "R1", quiet = TRUE),
      tobii = list(recording_id = "R2", quiet = TRUE)
    )
  )
  expect_s3_class(result, "eye_corpus_validation")
  expect_equal(nrow(result$summary), 2)
  expect_true(all(result$summary$status %in% c("pass", "warning")))
  matrix <- format_compatibility_matrix(result)
  expect_true(any(matrix$empirical_cases > 0))
})

test_that("validation corpora support expected rejections", {
  unsupported <- tempfile(fileext = ".idf")
  writeBin(as.raw(c(1, 2, 3, 4)), unsupported)
  manifest <- validation_manifest(
    unsupported,
    vendor = "smi",
    expected_import = FALSE,
    require_gaze = FALSE,
    require_native_time = FALSE,
    require_coordinate_space = FALSE,
    require_provenance = FALSE,
    run_roundtrip = FALSE,
    case_id = "unsupported-idf"
  )
  expect_warning(result <- validate_eye_corpus(manifest), NA)
  expect_true(result$summary$expectation_met)
  expect_equal(result$summary$status, "pass")
  unlink(unsupported)
})

test_that("validation manifests enforce unique cases and resolve relative paths", {
  root <- tempfile("eyeprocess-manifest-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  file.copy(extdata("gazepoint", "demo-user.csv"), file.path(root, "demo-user.csv"))
  manifest_path <- file.path(root, "validation-manifest.csv")
  manifest <- data.frame(
    case_id = "relative-gp",
    path = "demo-user.csv",
    vendor = "gazepoint",
    stringsAsFactors = FALSE
  )
  write_validation_manifest(manifest, manifest_path)
  restored <- read_validation_manifest(manifest_path)
  expect_true(file.exists(restored$path))
  expect_equal(basename(restored$path), "demo-user.csv")

  duplicated <- rbind(manifest, manifest)
  expect_error(write_validation_manifest(duplicated, file.path(root, "duplicate.csv")), "must be unique")
})

test_that("compatibility evidence is assigned to one exact format row", {
  manifest <- validation_manifest(
    extdata("gazepoint", "demo-user.csv"),
    vendor = "gazepoint",
    format_family = "gazepoint_analysis",
    case_id = "gp-exact"
  )
  result <- validate_eye_corpus(
    manifest,
    spec = format_validation_spec(run_roundtrip = FALSE),
    import_args = list("gp-exact" = list(recording_id = "R1", quiet = TRUE))
  )
  matrix <- format_compatibility_matrix(result)
  expect_equal(sum(matrix$empirical_cases), 1)
  expect_equal(matrix$empirical_cases[matrix$format_id == "gazepoint_analysis"], 1)
})

test_that("anonymized validation evidence removes paths and filenames", {
  source_path <- extdata("gazepoint", "demo-user.csv")
  result <- validate_eye_source(
    source_path,
    vendor = "gazepoint",
    spec = format_validation_spec(run_roundtrip = FALSE),
    import_args = list(recording_id = "secret-recording", quiet = TRUE),
    retain_dataset = TRUE,
    case_id = "privacy-case"
  )
  redacted <- eyeprocess:::.redact_validation_result(result)
  expect_equal(redacted$path, "<redacted>")
  expect_false(any(grepl(normalizePath(source_path, winslash = "/"), unlist(redacted$source), fixed = TRUE)))
  expect_false(any(redacted$source$file_name == basename(source_path)))
  expect_true(all(is.na(redacted$source$md5)))

  dataset <- anonymize_eye_dataset(result$dataset)
  expect_identical(dataset$vendor_metadata, list(redacted = TRUE))
})

test_that("manifest logical fields are parsed explicitly", {
  root <- tempfile("eyeprocess-flags-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  manifest_path <- file.path(root, "validation-manifest.csv")
  writeLines(
    c(
      "case_id,path,vendor,expected_import,require_gaze,run_roundtrip",
      "case-a,missing.csv,generic,yes,no,0"
    ),
    manifest_path
  )
  manifest <- read_validation_manifest(manifest_path)
  expect_true(manifest$expected_import)
  expect_false(manifest$require_gaze)
  expect_false(manifest$run_roundtrip)

  writeLines(
    c(
      "case_id,path,vendor,expected_import",
      "case-b,missing.csv,generic,perhaps"
    ),
    manifest_path
  )
  expect_error(read_validation_manifest(manifest_path), "invalid logical")
})

test_that("validation corpus initialization is safe and reproducible", {
  root <- tempfile("eyeprocess-corpus-")
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  created <- init_validation_corpus(root)
  manifest_path <- file.path(root, "validation-manifest.csv")
  readme_path <- file.path(root, "README.txt")
  sentinel_path <- file.path(root, "existing-case.csv")

  expect_true(dir.exists(created))
  expect_true(file.exists(manifest_path))
  expect_true(file.exists(readme_path))
  manifest <- read_validation_manifest(manifest_path)
  expect_equal(nrow(manifest), 0)

  manifest_before <- readLines(manifest_path, warn = FALSE)
  readme_before <- readLines(readme_path, warn = FALSE)
  writeLines("existing case data", sentinel_path, useBytes = TRUE)

  expect_message(
    repeated <- init_validation_corpus(root),
    "already initialized"
  )
  expect_identical(
    normalizePath(repeated, winslash = "/", mustWork = TRUE),
    normalizePath(root, winslash = "/", mustWork = TRUE)
  )
  expect_identical(readLines(manifest_path, warn = FALSE), manifest_before)
  expect_identical(readLines(readme_path, warn = FALSE), readme_before)
  expect_true(file.exists(sentinel_path))
})

test_that("validation_manifest accepts explicit logical spellings", {
  manifest <- validation_manifest(
    "missing.csv",
    vendor = "generic",
    expected_import = "yes",
    require_gaze = "no",
    run_roundtrip = "0",
    case_id = "logical-spellings"
  )
  expect_true(manifest$expected_import)
  expect_false(manifest$require_gaze)
  expect_false(manifest$run_roundtrip)
})

test_that("anonymized validation bundles exclude source paths and raw exports", {
  skip_if(!nzchar(Sys.which("zip")), "A zip executable is required for bundle testing.")
  source_path <- extdata("gazepoint", "demo-user.csv")
  result <- validate_eye_source(
    source_path,
    vendor = "gazepoint",
    spec = format_validation_spec(run_roundtrip = FALSE),
    import_args = list(recording_id = "bundle-secret", quiet = TRUE),
    retain_dataset = TRUE,
    case_id = "secret-case"
  )
  bundle <- tempfile(fileext = ".zip")
  extracted <- tempfile("eyeprocess-bundle-")
  on.exit(unlink(c(bundle, extracted), recursive = TRUE, force = TRUE), add = TRUE)
  create_validation_bundle(result, bundle, anonymize = TRUE, overwrite = TRUE)
  utils::unzip(bundle, exdir = extracted)
  report <- paste(readLines(file.path(extracted, "validation-report.md"), warn = FALSE), collapse = "\n")
  source_manifest <- utils::read.csv(
    file.path(extracted, "source-manifest.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  expect_false(grepl(normalizePath(source_path, winslash = "/"), report, fixed = TRUE))
  expect_false(grepl(basename(source_path), report, fixed = TRUE))
  expect_true(all(source_manifest$source_path == "<redacted>"))
  expect_true(all(is.na(source_manifest$md5)))
  expect_false(any(basename(list.files(extracted, recursive = TRUE)) == "raw.rds"))
})


test_that("validation corpus initialization is idempotent and non-destructive", {
  root <- tempfile("eyeprocess-corpus-init-")
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  first <- init_validation_corpus(root)
  marker <- file.path(root, "case-file.csv")
  writeLines("x\n1", marker)
  manifest_before <- readLines(file.path(root, "validation-manifest.csv"), warn = FALSE)
  expect_warning(second <- init_validation_corpus(root), NA)
  expect_identical(normalizePath(first, winslash = "/"), normalizePath(second, winslash = "/"))
  expect_true(file.exists(marker))
  expect_identical(readLines(file.path(root, "validation-manifest.csv"), warn = FALSE), manifest_before)
})
