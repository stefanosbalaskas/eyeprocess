testthat::test_that("visual context registry identifies shared contexts", {
  m <- data.frame(item_id = paste0("i", 1:8),
                  screen_id = c(rep("screen_A", 4), paste0("u", 5:8)))
  r <- visual_context_registry(m, context = "screen_id")
  testthat::expect_s3_class(r, "eye_visual_context_registry")
  testthat::expect_true(any(r$mapping$shared_context))
})

testthat::test_that("visual-context IRT fits when mirt is available", {
  testthat::skip_if_not_installed("mirt")
  set.seed(6)
  X <- matrix(rbinom(200 * 8, 1, .6), 200, 8)
  colnames(X) <- paste0("i", 1:8)
  m <- data.frame(item_id = colnames(X), screen_id = c(rep("screen_A", 4), paste0("u", 5:8)))
  r <- visual_context_registry(m, context = "screen_id")
  fit <- fit_visual_context_irt(X, r)
  testthat::expect_s3_class(fit, "eye_visual_context_irt")
  testthat::expect_equal(length(fit$positions), 4)
})

testthat::test_that("multiblock mapping has a base fallback", {
  set.seed(7)
  d <- data.frame(person_id = paste0("P", 1:30),
                  theta = rnorm(30), accuracy = runif(30),
                  dwell = rnorm(30, 700, 70), entropy = runif(30),
                  pupil = rnorm(30), validity = runif(30, .8, 1))
  b <- process_feature_blocks(d, list(Psychometric = c("theta", "accuracy"),
                                      Gaze = c("dwell", "entropy"),
                                      Pupil = "pupil", Quality = "validity"), id = "person_id")
  fit <- fit_multiblock_process_map(b, engine = "pca_block_scaled")
  testthat::expect_s3_class(fit, "eye_multiblock_process_map")
  testthat::expect_equal(nrow(multiblock_person_coordinates(fit)), 30)
})

testthat::test_that("process-profile reference is explicitly descriptive", {
  set.seed(8)
  d <- data.frame(person_id = paste0("P", 1:50), a = rnorm(50), b = rnorm(50), c = rnorm(50))
  fit <- fit_process_profile_mixture(d, c("a", "b", "c"), k = 3, engine = "kmeans_reference")
  testthat::expect_s3_class(fit, "eye_process_profile_mixture")
  testthat::expect_match(fit$status, "not_finite_mixture")
  testthat::expect_equal(nrow(process_profile_probabilities(fit)), 50)
})
