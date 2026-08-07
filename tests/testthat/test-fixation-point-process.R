test_that("fixation point-process intensity and diagnostics work", {
  gaze <- mi_gaze_data(100)
  gaze$duration <- rexp(100, 1 / 200)
  fit <- fit_fixation_point_process(gaze, interaction = "self_exciting", grid_size = 8)
  expect_s3_class(fit, "eye_fixation_point_process")
  predicted <- predict_fixation_intensity(fit)
  expect_true(all(predicted$predicted_intensity >= 0))
  marked <- fit_marked_gaze_process(gaze, marks = c("duration", "pupil"))
  expect_s3_class(marked, "eye_marked_gaze_process")
  diagnostics <- diagnose_gaze_point_process(fit)
  expect_s3_class(diagnostics, "eye_gaze_point_process_diagnostics")
  expect_plot_silent(plot_fixation_intensity(fit))
})

test_that("point-process engine handles constant coordinate ranges and covariates", {
  data <- data.frame(
    x = rep(0.5, 30),
    y = rep(0.5, 30),
    time = seq_len(30),
    salience = seq(0, 1, length.out = 30)
  )
  fit <- fit_fixation_point_process(data, spatial_covariates = "salience", grid_size = 5)
  expect_s3_class(fit, "eye_fixation_point_process")
  expect_true("salience" %in% fit$covariate_map$source)
  predicted <- predict_fixation_intensity(fit)
  expect_true(all(is.finite(predicted$predicted_intensity)))
})
