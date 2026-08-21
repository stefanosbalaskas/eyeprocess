testthat::test_that("M4 negative controls are deterministic design objects", {
  sim <- simulate_multimodal_m4(n_person = 10L, n_item = 6L, seed = 106L)

  a <- multimodal_m4_negative_controls(sim, seed = 99L, run = FALSE)
  b <- multimodal_m4_negative_controls(sim, seed = 99L, run = FALSE)

  expected <- c(
    "order_shuffle",
    "process_shuffle",
    "state_independent",
    "nuisance_pseudostate",
    "device_session_pseudostate",
    "overfit_state_count"
  )

  testthat::expect_s3_class(
    a,
    "eye_multimodal_m4_negative_controls"
  )

  testthat::expect_false(a$executed)
  testthat::expect_false(b$executed)
  testthat::expect_identical(a$controls, expected)
  testthat::expect_identical(a$controls, b$controls)
  testthat::expect_equal(length(a$controls), 6L)
  testthat::expect_identical(a$data, b$data)
})
