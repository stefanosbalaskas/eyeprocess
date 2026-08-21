testthat::test_that("M4 simulation is deterministic and preserves truth", {
  a <- simulate_multimodal_m4(n_person = 10L, n_item = 6L, seed = 101L)
  b <- simulate_multimodal_m4(n_person = 10L, n_item = 6L, seed = 101L)
  testthat::expect_identical(a$data, b$data)
  testthat::expect_identical(a$truth$state, b$truth$state)
  n <- simulate_multimodal_m4(n_person = 10L, n_item = 6L, scenario = "null", seed = 102L)
  testthat::expect_identical(n$truth$n_states, 1L)
})
