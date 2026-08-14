test_that("process information reports posterior uncertainty reduction", {
  set.seed(1)
  b <- matrix(rnorm(4000, sd=1), ncol=2)
  a <- matrix(rnorm(4000, sd=.7), ncol=2)
  colnames(b) <- colnames(a) <- c("p1","p2")
  z <- process_information(b, a, "variance_reduction")
  expect_s3_class(z, "eye_process_information")
  expect_true(all(z$value > 0))
  expect_true(all(z$relative_variance_reduction > 0))
})

test_that("channel ablation retains stable measurement rows", {
  sim <- simulate_multimodal_irt(n_person=15,n_item=5,seed=3)
  ab <- ablate_multimodal_channels(sim$measurement)
  expect_s3_class(ab, "eye_multimodal_ablation")
  expect_equal(length(ab$scenarios), 8)
  n <- vapply(ab$scenarios, function(z) nrow(z$data), integer(1))
  expect_true(length(unique(n)) == 1L)
})
