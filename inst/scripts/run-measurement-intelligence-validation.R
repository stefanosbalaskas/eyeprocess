args <- commandArgs(trailingOnly = TRUE)
package_path <- if (length(args)) args[[1L]] else "."
package_path <- normalizePath(package_path, winslash = "/", mustWork = TRUE)

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Package `testthat` is required.", call. = FALSE)
}

r_files <- list.files(
  file.path(package_path, "R"),
  pattern = "^(03[0-9]|04[0-7]).*\\.[Rr]$",
  full.names = TRUE
)
parse_failures <- lapply(r_files, function(path) {
  tryCatch({ parse(path); NULL }, error = identity)
})
bad <- r_files[vapply(parse_failures, inherits, logical(1), "error")]
if (length(bad)) {
  stop("Measurement-intelligence parse failures: ", paste(basename(bad), collapse = ", "), call. = FALSE)
}

results <- testthat::test_dir(
  file.path(package_path, "tests", "testthat"),
  filter = "probabilistic|compositional|uncertainty|calibration|reliability|device|pupil|missingness|recurrence|point-process|scanpath|episodes|item-bank|process-dif|process-norms|evidence|measurement-intelligence",
  reporter = "summary",
  stop_on_failure = TRUE,
  stop_on_warning = TRUE
)

message("Measurement-intelligence parse and targeted-test validation completed.")
invisible(results)
