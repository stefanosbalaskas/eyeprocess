test_that("observation and MNAR sensitivity models work", {
  data <- mi_trial_data()
  observation <- fit_process_observation_model(data, "observed", c("dwell_ms", "pupil"))
  expect_s3_class(observation, "eye_process_observation_model")
  joint <- fit_joint_signal_missingness("pupil", observation, x = data, predictors = "dwell_ms")
  expect_s3_class(joint, "eye_joint_signal_missingness")
  values <- data$pupil; values[data$observed == 0] <- NA
  sensitivity <- process_pattern_mixture(values, delta = c(-1, 0, 1))
  expect_equal(nrow(sensitivity$table), 3)
  tipping <- sensitivity_mnar_process(sensitivity)
  expect_s3_class(tipping, "eye_mnar_tipping_point")
  expect_plot_silent(plot_mnar_tipping_point(tipping))
})
