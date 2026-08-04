test_that("preprocessing specifications are explicit and provenance-aware", {
  x <- simulate_eye_dataset(n_person = 3, n_item = 3, samples_per_trial = 30, seed = 1)
  spec <- preprocess_spec(gaze_filter = "moving_average", pupil_filter = "moving_average", fixation_algorithm = "ivt", fixation_parameters = list(velocity_threshold = 1000, minimum_duration_ms = 0, coordinate_units = "degrees"))
  expect_no_warning(y <- preprocess_eye(x, spec))
  expect_s3_class(y, "eye_dataset")
  expect_true(any(y$provenance$action == "preprocess_eye"))
  expect_true(any(y$episodes$source_algorithm == "I-VT"))
})
