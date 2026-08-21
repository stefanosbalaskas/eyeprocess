testthat::test_that("M4 backend-free plots return ggplot objects", {
  testthat::skip_if_not_installed("ggplot2")
  sim <- simulate_multimodal_m4(n_person = 10L, n_item = 6L, seed = 107L)
  states <- multimodal_m4_state_diagnostics(sim)
  audit <- audit_multimodal_m4_identifiability(sim, include_posterior = FALSE)
  testthat::expect_s3_class(plot(sim), "ggplot")
  testthat::expect_s3_class(plot(states), "ggplot")
  testthat::expect_s3_class(plot(audit), "ggplot")
})
