testthat::test_that("M4 recovery is five-scenario and inert by default", {
  r <- multimodal_m4_recovery()
  testthat::expect_s3_class(r, "eye_multimodal_m4_recovery")
  testthat::expect_false(r$executed)
  testthat::expect_equal(nrow(r$design), 5L)
})
