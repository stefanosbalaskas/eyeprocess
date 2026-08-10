test_that("0.9 process registry is guarded and reliability is finite", {
  reg <- process_measure_registry()
  expect_s3_class(reg, "eye_process_measure_registry")
  expect_true("pupil_peak" %in% reg$name)
  expect_true(nzchar(process_measure_card("pupil_peak")$guardrail))
  set.seed(9)
  d <- expand.grid(person_id=1:30, session=1:2)
  u <- rnorm(30)
  d$value <- rep(u, each=2) + rnorm(nrow(d), sd=.2)
  icc <- process_icc(d, "person_id", "session", "value")
  expect_true(is.finite(icc$icc_a1))
  ba <- process_bland_altman(d, "person_id", "session", "value")
  expect_s3_class(ba, "eye_process_bland_altman")
})

test_that("0.9 process-measure registry rejects incomplete or vector metadata", {
  reg <- process_measure_registry()
  bad <- reg
  bad$guardrail[[1L]] <- NA_character_
  expect_error(validate_process_measure_registry(bad), "requires non-missing")
  expect_error(
    register_process_measure(
      reg,
      name = c("x", "y"),
      channel = "gaze",
      unit = "a.u.",
      level = "trial",
      interpretation = "Neutral process feature.",
      guardrail = "Not a psychological diagnosis."
    ),
    "must be scalar"
  )
})
