test_that("Data Quality site pages and API references remain discoverable", {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
  pkgdown <- paste(readLines(file.path(root, "_pkgdown.yml"), warn = FALSE), collapse = "\n")

  expect_match(pkgdown, "standardized-data-quality", fixed = TRUE)
  expect_match(pkgdown, "data-quality-plot-gallery", fixed = TRUE)

  required <- c(
    "vignettes/standardized-data-quality.Rmd",
    "vignettes/data-quality-plot-gallery.Rmd",
    "man/standardized_spatial_quality.Rd",
    "man/multilevel-mediation.Rd"
  )
  expect_true(all(file.exists(file.path(root, required))))

  exports <- getNamespaceExports("eyeprocess")
  expect_true(all(c(
    "create_gaze_quality_report",
    "plot_gaze_accuracy",
    "plot_gaze_precision",
    "plot_bcea",
    "plot_sampling_intervals",
    "plot_gaze_quality_dashboard",
    "report_gaze_quality",
    "prepare_multilevel_mediation_data",
    "simulate_gaze_survival_inputs"
  ) %in% exports))
})
