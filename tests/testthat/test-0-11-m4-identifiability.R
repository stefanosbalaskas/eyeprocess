testthat::test_that("M4 identifiability is multi-criterion", {
  sim <- simulate_multimodal_m4(n_person = 12L, n_item = 6L, seed = 104L)
  a <- audit_multimodal_m4_identifiability(sim, include_posterior = FALSE)

  testthat::expect_s3_class(a, "eye_multimodal_m4_identifiability")

  testthat::expect_true(all(c(
    "domain", "criterion", "status", "severity",
    "value", "threshold", "message", "recommendation"
  ) %in% names(a$checks)))

  testthat::expect_true(
    a$overall %in% c(
      "PASS", "PASS_WITH_CAUTION", "REVIEW",
      "FAIL", "NOT_EVALUATED"
    )
  )

  testthat::expect_true(is.logical(a$supported))
  testthat::expect_length(a$supported, 1L)
})
