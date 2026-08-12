test_that("joint process IRT specifications retain measurement boundaries", {
  s <- eyeprocess_joint_process_irt_spec(response_family = "2pl", time_model = "lognormal", process_channels = c("pupil","gaze"))
  expect_s3_class(s, "eye_joint_process_irt_spec")
  expect_true(validate_eyeprocess_joint_process_irt_spec(s))
  map <- eyeprocess_multichannel_measurement_map(response = "accuracy", channels = c("rt","pupil","gaze"))
  expect_true(all(c("channel","role","inference_boundary") %in% names(map)))
})
