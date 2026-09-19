test_that("gaze-survival raw simulator remains part of the public contract", {
  expect_true("simulate_gaze_survival_inputs" %in% getNamespaceExports("eyeprocess"))
  expect_true(is.function(getExportedValue("eyeprocess", "simulate_gaze_survival_inputs")))
})

test_that("gaze-survival site contract remains discoverable", {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
  pkgdown <- file.path(root, "_pkgdown.yml")

  expect_true(file.exists(pkgdown))
  text <- paste(readLines(pkgdown, warn = FALSE), collapse = "\n")

  expect_match(text, "gaze-survival-analysis", fixed = TRUE)
  expect_match(text, "gaze-survival-verification-example", fixed = TRUE)
  expect_match(text, "gaze-survival-reproducibility-checklist", fixed = TRUE)
  expect_match(text, "Censored gaze-latency survival analysis", fixed = TRUE)
  expect_match(text, "prepare_gaze_survival_data", fixed = TRUE)
  expect_match(text, "fit_gaze_mixed_cox_model", fixed = TRUE)
  expect_match(text, "fit_gaze_aft_model", fixed = TRUE)
  expect_match(text, "report_gaze_survival_model", fixed = TRUE)
})

test_that("gaze-survival examples and articles remain in the source tree", {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
  required <- c(
    "R/114-gaze-survival.R",
    "man/gaze-survival.Rd",
    "inst/examples/worked_gaze_survival_analysis.R",
    "inst/examples/worked_gaze_verification_survival.R",
    "vignettes/gaze-survival-analysis.Rmd",
    "vignettes/gaze-survival-verification-example.Rmd",
    "vignettes/gaze-survival-reproducibility-checklist.Rmd"
  )
  expect_true(all(file.exists(file.path(root, required))))
})
