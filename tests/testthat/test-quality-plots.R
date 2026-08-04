test_that("quality and governance outputs are structured", {
  x <- simulate_eye_dataset(n_person = 3, n_item = 3, samples_per_trial = 20, seed = 2)
  expect_true(nrow(audit_sampling_rate(x)) > 0)
  expect_true(nrow(audit_signal_quality(x)) > 0)
  expect_s3_class(analysis_readiness(x), "eye_readiness")
  warnings <- interpretive_warnings()
  expect_true(nrow(warnings) >= 4)
  expect_true(all(c("warning_id", "observation", "guidance") %in% names(warnings)))
})

test_that("base plots execute on a graphics device", {
  x <- simulate_eye_dataset(n_person = 2, n_item = 2, samples_per_trial = 20, seed = 3)
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  on.exit({grDevices::dev.off(); unlink(f)}, add = TRUE)
  expect_invisible(plot_eye_overview(x))
  expect_invisible(plot_eye_trace(x, trial_id = x$intervals$trial_id[1]))
  expect_invisible(plot_pupil_timeseries(x, trial_id = x$intervals$trial_id[1]))
})
