test_that("functional pupil bridge requires an explicit trial-level score", {
  sim <- simulate_multimodal_m3(n_person=25,n_item=6,seed=7)
  d <- sim$data
  d$trajectory_score <- seq_len(nrow(d)) / nrow(d)
  b <- multimodal_m3_functional_bridge(d, "trajectory_score", provenance="score from preregistered trajectory basis")
  expect_s3_class(b, "eye_multimodal_m3_functional_bridge")
  expect_equal(b$data$pupil, d$trajectory_score)
  expect_identical(b$representation, "functional_score")
  expect_match(b$boundary, "does not claim")
  expect_error(multimodal_m3_functional_bridge(d, 1:2), "one finite-or-NA value per trial")
})
