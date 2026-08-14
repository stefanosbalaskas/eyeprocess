test_that("M2 specification reuses established multimodal architecture", {
  s <- multimodal_m2_spec()

  expect_s3_class(s, "eye_multimodal_m2_spec")
  expect_true(inherits(s, "eye_multimodal_irt_spec"))
  expect_true(inherits(s, "eye_irt_model_spec"))
  expect_identical(s$model, "M2")
  expect_identical(s$backend, "cmdstanr")
  expect_setequal(names(s$channels), c("response", "rt", "gaze"))
  expect_identical(s$reference$doi, "10.1177/01466216221089344")
  expect_match(s$fidelity$response, "Rasch")
})

test_that("M2 specification rejects unsupported missingness semantics", {
  expect_error(
    multimodal_m2_spec(missingness = "MNAR"),
    "ignorable"
  )
})
