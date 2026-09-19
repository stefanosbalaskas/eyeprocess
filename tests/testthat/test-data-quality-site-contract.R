test_that("Data Quality site pages remain discoverable in the source tree", {
  root <- eyeprocess_source_root()
  if (is.null(root)) {
    skip("Source-tree pkgdown assets are unavailable in installed-package context.")
  }
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
})

test_that("Data Quality public API remains discoverable", {
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
