testthat::test_that("M4 state diagnostics retain probabilities", {
  sim <- simulate_multimodal_m4(n_person = 10L, n_item = 6L, seed = 103L)
  s <- multimodal_m4_state_diagnostics(sim)

  testthat::expect_s3_class(s, "eye_multimodal_m4_states")
  testthat::expect_identical(s$source, "synthetic_truth")

  pcols <- grep(
    "^state_[0-9]+_probability$",
    names(s$probability),
    value = TRUE
  )

  testthat::expect_equal(
    length(pcols),
    sim$truth$n_states
  )

  testthat::expect_equal(
    rowSums(s$probability[pcols]),
    rep(1, nrow(s$probability)),
    tolerance = 1e-12
  )

  testthat::expect_true(
    "posterior_entropy" %in% names(s$probability)
  )

  testthat::expect_true(
    "MAP_state" %in% names(s$probability)
  )
})
