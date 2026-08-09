testthat::test_that("latent class to process alignment remains descriptive", {
  set.seed(13)
  cls <- data.frame(person_id = paste0("P", 1:20), class = rep(c("A", "B"), each = 10))
  proc <- data.frame(person_id = rep(paste0("P", 1:20), each = 2),
                     dwell = rnorm(40), pupil = rnorm(40))
  x <- map_latent_classes_to_process_profiles(cls, proc,
                                               process_features = c("dwell", "pupil"))
  testthat::expect_s3_class(x, "eye_latent_process_alignment")
  testthat::expect_equal(nrow(x$summary), 2)
  testthat::expect_match(x$caveat, "cannot prove")
})

testthat::test_that("biometric imputation sensitivity can report missingness without engines", {
  d <- data.frame(a = c(1, NA, 3), b = c(NA, 2, 3))
  x <- biometric_imputation_sensitivity(d, c("a", "b"), methods = character())
  testthat::expect_s3_class(x, "eye_biometric_imputation_sensitivity")
  testthat::expect_equal(nrow(x$missingness), 2)
  testthat::expect_equal(length(x$results), 0)
})

testthat::test_that("true mixture IRT interface is conditional on mirt", {
  testthat::skip_if_not_installed("mirt")
  set.seed(14)
  X <- matrix(rbinom(300 * 8, 1, .55), 300, 8)
  colnames(X) <- paste0("i", 1:8)
  fit <- fit_mixture_irt_process_classes(X, n_classes = 2)
  testthat::expect_s3_class(fit, "eye_mixture_irt_process")
  testthat::expect_equal(fit$n_classes, 2)
})
