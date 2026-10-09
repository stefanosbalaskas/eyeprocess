test_that("acquisition intervals keep timestamp irregularities explicit", {
  time <- c(0, .01, .02, .02, .06, .061, .07)
  a <- audit_eye_acquisition_integrity(
    time, valid = c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE),
    device_native_rate_hz = 100, sdk_delivered_rate_hz = 90,
    analysis_grid_rate_hz = 500
  )
  expect_s3_class(a, "eye_acquisition_integrity")
  expect_equal(a$summary$quantized_or_duplicate_timestamps, 1)
  expect_equal(a$summary$invalid_state_fraction, 1/7)
  expect_true(a$summary$upsampling_does_not_add_information)
  expect_true(a$summary$catchup_bursts >= 1)
  expect_error(audit_eye_acquisition_integrity(c(0, .2, .1)), "reversal")
  expect_error(audit_eye_acquisition_integrity(c(0, .1, .2),
                                               valid = c(TRUE, NA, TRUE)), "valid")
})
test_that("acquisition interval plot is optional and retains raw units", {
  a <- audit_eye_acquisition_integrity(seq(0, .2, by = .01))
  skip_if_not_installed("ggplot2")
  expect_s3_class(plot_eye_acquisition_integrity(a), "ggplot")
  expect_error(plot_eye_acquisition_integrity(list()), "eye_acquisition_integrity")
})
