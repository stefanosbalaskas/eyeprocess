test_that("compatibility aliases and cross-modal model work", {
  data <- mi_trial_data()
  missingness <- fit_process_missingness_model(data, "observed", c("dwell_ms", "pupil"))
  expect_s3_class(missingness, "eye_process_observation_model")
  model <- crossmodal_recurrence_model(seq_len(30), sin(seq_len(30) / 5))
  expect_s3_class(model, "eye_crossmodal_recurrence_model")
  expect_true("recurrence_rate" %in% names(model$features))
  expect_plot_silent(plot(model, type = "crossmodal"))
})
