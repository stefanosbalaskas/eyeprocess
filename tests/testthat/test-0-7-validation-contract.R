test_that("recovery summary computes bias RMSE and coverage", {
  d <- data.frame(
    replicate = rep(1:4, each = 2),
    parameter = rep(c("a", "b"), 4),
    truth = rep(c(1, 0), 4),
    estimate = c(1.1, .1, .9, -.1, 1.05, .05, .95, -.05),
    lower = c(.5, -.5, .5, -.5, .5, -.5, .5, -.5),
    upper = c(1.5, .5, 1.5, .5, 1.5, .5, 1.5, .5),
    converged = TRUE
  )
  s <- summarize_parameter_recovery(d)
  expect_true(all(c("bias", "rmse", "coverage", "failure_rate") %in% names(s)))
  expect_true(all(is.finite(s$rmse)))
  expect_true(all(s$coverage == 1))
  expect_true(all(audit_convergence(d)$pass))
})

test_that("SBC ranks and audits use normalized ranks", {
  sim <- function(r) list(data = rnorm(5), truth = c(mu = 0))
  fit <- function(dat) list(draws = matrix(rnorm(200, mean(dat), 1), ncol = 1,
                                          dimnames = list(NULL, "mu")))
  dr <- function(fit) fit$draws
  z <- run_sbc(sim, fit, dr, replications = 8, seed = 1)
  expect_s3_class(z, "eye_irt_sbc")
  expect_true(all(z$ranks$normalized_rank > 0 & z$ranks$normalized_rank < 1))
  a <- audit_sbc(z, bins = 4)
  expect_true("pass_screen" %in% names(a))
})

test_that("posterior predictive discrepancy keeps tail probabilities", {
  obs <- 1:10
  reps <- lapply(1:20, function(i) 1:10 + rnorm(10, 0, .2))
  z <- posterior_predictive_discrepancies(obs, reps,
                                          discrepancies = list(mean = mean, sd = sd))
  expect_equal(nrow(z), 2)
  expect_true(all(z$p_two_sided >= 0 & z$p_two_sided <= 1))
})

test_that("negative control preserves result contract", {
  d <- data.frame(y = rnorm(30), process = rnorm(30), person = rep(1:10, each = 3))
  evaluator <- function(x) cor(x$y, x$process)
  z <- negative_control_process_test(d, "process", evaluator,
                                     within = "person", permutations = 10, seed = 4)
  expect_s3_class(z, "eye_process_negative_control")
  expect_equal(length(z$null), 10)
  expect_true(z$p_value > 0 && z$p_value <= 1)
})
