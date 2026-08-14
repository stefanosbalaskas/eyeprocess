test_that("M0-M2 Stan programs are packaged and M2 likelihood terms are explicit", {
  f0 <- eyeprocess:::.ep10_m2_stan_file("M0")
  f1 <- eyeprocess:::.ep10_m2_stan_file("M1")
  f2 <- eyeprocess:::.ep10_m2_stan_file("M2")

  expect_true(file.exists(f0))
  expect_true(file.exists(f1))
  expect_true(file.exists(f2))

  x <- readLines(f2, warn = FALSE)

  expect_true(any(grepl("bernoulli_logit", x, fixed = TRUE)))
  expect_true(any(grepl("normal_lpdf", x, fixed = TRUE)))
  expect_true(any(grepl("neg_binomial_2_log_lpmf", x, fixed = TRUE)))
  expect_true(any(grepl("corr_person", x, fixed = TRUE)))
  expect_true(any(grepl("corr_item", x, fixed = TRUE)))
})
