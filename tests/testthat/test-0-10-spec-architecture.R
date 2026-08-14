test_that("multimodal specification delegates to the established IRT architecture", {
  response <- irt_response_channel()
  rt <- irt_rt_channel()
  gaze <- irt_count_channel(latent = "gaze_process")

  s <- multimodal_irt_spec(
    response = response,
    rt = rt,
    gaze = gaze,
    model = "M2",
    backend = "cmdstanr",
    identification = list(person = "standard_normal"),
    priors = list(source = "development")
  )

  expect_s3_class(s, "eye_multimodal_irt_spec")
  expect_true(inherits(s, "eye_irt_model_spec"))
  expect_identical(s$model, "M2")
  expect_identical(s$backend, "cmdstanr")
  expect_identical(s$lifecycle_status, "gated")
  expect_setequal(names(s$channels), c("response", "rt", "gaze"))
  expect_identical(s$metadata$architecture, "irt_model_spec_adapter")
  expect_identical(s$metadata$identification, list(person = "standard_normal"))
  expect_identical(s$metadata$priors, list(source = "development"))

  calls <- all.names(body(multimodal_irt_spec), functions = TRUE)
  expect_true("irt_model_spec" %in% calls)
})

test_that("multimodal specification rejects parallel ad hoc channel lists", {
  fake <- list(latent = "ability")

  expect_error(
    multimodal_irt_spec(response = fake, model = "M0"),
    "eye_irt_channel"
  )
})
