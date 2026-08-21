testthat::test_that("M4 Stan program is marginalized and uncertainty-first", {
  f <- system.file("stan", "m4-trait-conditioned-state-0-11.stan", package = "eyeprocess")
  testthat::expect_true(nzchar(f))
  x <- readLines(f, warn = FALSE)
  testthat::expect_true(any(grepl("log_sum_exp", x, fixed = TRUE)))
  testthat::expect_true(any(grepl("matrix[N, K] state_prob", x, fixed = TRUE)))
  testthat::expect_false(any(grepl("int<lower=1, upper=K> z", x, fixed = TRUE)))
})
