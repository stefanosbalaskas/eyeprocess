testthat::test_that("M4 sequence contracts are explicit", {
  sim <- simulate_multimodal_m4(n_person = 8L, n_item = 6L, seed = 11L)
  a <- audit_multimodal_m4_identifiability(sim, include_posterior = FALSE)
  testthat::expect_s3_class(a, "eye_multimodal_m4_identifiability")
  testthat::expect_true(a$supported)
  bad <- sim$data
  bad <- bad[c(2,1,seq.int(3,nrow(bad))), , drop = FALSE]
  testthat::expect_error(audit_multimodal_m4_identifiability(bad, include_posterior = FALSE), "order|sorted|trial|sequence")
})
