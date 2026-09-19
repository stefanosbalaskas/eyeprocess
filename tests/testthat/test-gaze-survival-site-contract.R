test_that("gaze-survival raw simulator remains part of the public contract", {
  expect_true("simulate_gaze_survival_inputs" %in% getNamespaceExports("eyeprocess"))
  expect_true(is.function(getExportedValue("eyeprocess", "simulate_gaze_survival_inputs")))
})

test_that("gaze-survival site contract remains discoverable in source tree", {
  root <- eyeprocess_source_root()
  if (is.null(root)) {
    skip("Source-tree pkgdown assets are unavailable in installed-package context.")
  }
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

test_that("gaze-survival source examples and articles remain available", {
  root <- eyeprocess_source_root()
  if (is.null(root)) {
    skip("Source-tree vignette assets are unavailable in installed-package context.")
  }
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

test_that("gaze-survival installed examples and API remain available", {
  examples <- c(
    system.file("examples", "worked_gaze_survival_analysis.R", package = "eyeprocess"),
    system.file("examples", "worked_gaze_verification_survival.R", package = "eyeprocess")
  )
  expect_true(all(nzchar(examples)))
  expect_true(all(file.exists(examples)))
  exports <- getNamespaceExports("eyeprocess")
  expect_true(all(c(
    "prepare_gaze_survival_data",
    "fit_gaze_mixed_cox_model",
    "fit_gaze_aft_model",
    "report_gaze_survival_model"
  ) %in% exports))
})
