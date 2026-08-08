# Comprehensive local validation for eyeprocess.
package_path <- normalizePath(
  "path/to/eyeprocess",
  winslash = "/",
  mustWork = TRUE
)

required <- c("devtools", "testthat", "knitr", "rmarkdown", "jsonlite", "pkgdown")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)

old <- setwd(package_path)
on.exit(setwd(old), add = TRUE)

# Remove known scaffold artefacts that can survive overwrite-only extraction.
stale_scaffold <- c("R/hello.R", "man/hello.Rd", "tests/testthat/test-hello.R")
stale_scaffold <- stale_scaffold[file.exists(stale_scaffold)]
if (length(stale_scaffold)) {
  unlink(stale_scaffold, force = TRUE)
  cat(
    "Removed stale scaffold file(s):\n",
    paste(" -", stale_scaffold, collapse = "\n"),
    "\n",
    sep = ""
  )
}

cat("\n== Documentation ==\n")
if (requireNamespace("roxygen2", quietly = TRUE)) {
  # Existing NAMESPACE and Rd files are complete; this verifies parseability of
  # source files without replacing the manual documentation.
  invisible(lapply(list.files("R", full.names = TRUE, pattern = "\\.R$"), parse))
}

cat("\n== Unit tests ==\n")
devtools::test(reporter = "summary", stop_on_failure = TRUE)

cat("\n== Package check ==\n")
check <- devtools::check(
  document = FALSE,
  manual = FALSE,
  cran = FALSE,
  error_on = "note"
)

cat("\n== pkgdown configuration ==\n")
pkgdown::check_pkgdown()

cat("\n== Installation ==\n")
if ("package:eyeprocess" %in% search()) {
  detach("package:eyeprocess", unload = TRUE, character.only = TRUE)
}
if ("eyeprocess" %in% loadedNamespaces()) {
  try(unloadNamespace("eyeprocess"), silent = TRUE)
}
install.packages(package_path, repos = NULL, type = "source")

cat("\n== Runtime smoke tests ==\n")
library(eyeprocess)
x <- simulate_eye_dataset(
  n_person = 4,
  n_item = 4,
  sampling_rate = 30,
  trial_duration = 0.5,
  seed = 2026
)
x <- preprocess_eye(
  x,
  preprocess_spec(
    gaze_filter = "median",
    pupil_filter = "median",
    fixation_algorithm = "ivt",
    fixation_parameters = list(
      velocity_threshold = 1000,
      minimum_duration_ms = 0
    )
  )
)
x <- build_aoi_visits(x)
x <- derive_all_features(x)
validation <- validate_eye_dataset(x)
stopifnot(
  inherits(x, "eye_dataset"),
  nrow(validation[validation$severity == "error", , drop = FALSE]) == 0L,
  nrow(x$features) > 0L,
  nrow(x$provenance) > 0L
)

format_profiles <- eye_format_profiles()
stopifnot(
  nrow(format_profiles) >= 12L,
  all(supported_eye_formats()$has_validator)
)
fixture <- system.file("extdata", "gazepoint", "demo-user.csv", package = "eyeprocess")
format_validation <- validate_eye_source(
  fixture,
  vendor = "gazepoint",
  spec = format_validation_spec(run_roundtrip = TRUE),
  import_args = list(recording_id = "R_VALIDATION", quiet = TRUE),
  retain_dataset = FALSE,
  case_id = "runtime-gazepoint-fixture"
)
stopifnot(
  inherits(format_validation, "eye_format_validation"),
  format_validation$status != "fail",
  any(format_validation$checks$check == "canonical_roundtrip" &
        format_validation$checks$status == "pass")
)

workflow_fixture <- system.file("extdata", "gazepoint_v72", package = "eyeprocess")
workflow_output <- tempfile("eyeprocess-downstream-smoke-")
workflow <- run_gazepoint_workflow(
  workflow_fixture,
  output_dir = workflow_output,
  spec = gazepoint_workflow_spec(
    create_plots = FALSE,
    create_html_report = FALSE,
    retain_raw = FALSE
  ),
  overwrite = TRUE,
  quiet = TRUE
)
workflow_checks <- validate_gazepoint_workflow(workflow)
stopifnot(
  inherits(workflow, "eye_gazepoint_workflow"),
  workflow$status == "pass",
  nrow(workflow$tables$process) == nrow(workflow$tables$trials),
  all(workflow_checks$passed)
)
unlink(workflow_output, recursive = TRUE, force = TRUE)

cat("\nAll requested local validation stages completed.\n")
print(sessionInfo())
