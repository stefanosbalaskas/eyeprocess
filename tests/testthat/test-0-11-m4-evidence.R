testthat::test_that("M4 evidence workflows are bounded by default", {
  sim <- simulate_multimodal_m4(n_person = 10L, n_item = 6L, seed = 105L)
  abl <- multimodal_m4_ablation(sim)
  sens <- multimodal_m4_sensitivity(sim, n_states = 1:3)
  testthat::expect_false(abl$executed)
  testthat::expect_false(sens$executed)
})
