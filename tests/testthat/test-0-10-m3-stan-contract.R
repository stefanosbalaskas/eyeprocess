test_that("M3 Stan contracts preserve four-channel Cholesky architecture", {
  full <- readLines(.ep10_m3_stan_file("full"), warn=FALSE)
  subset <- readLines(.ep10_m3_stan_file("ablation"), warn=FALSE)
  expect_true(any(grepl("cholesky_factor_corr\\[4\\] L_person", full)))
  expect_true(any(grepl("cholesky_factor_corr\\[4\\] L_item", full)))
  expect_true(any(grepl("matrix\\[4, 4\\] corr_person", full)))
  expect_false(any(grepl("corr_matrix\\[4\\] corr_person", full)))
  expect_true(any(grepl("vector\\[8\\] gamma_pupil", full)))
  expect_true(any(grepl("matrix\\[N_pupil, 8\\] X_pupil", full)))
  expect_true(any(grepl("array\\[8\\].*use_pupil_covariate", full)))
  expect_true(any(grepl("log_lik_pupil", full)))
  expect_true(any(grepl("P_obs", full)))
  expect_true(any(grepl("use_pupil", subset)))
  expect_true(any(grepl("K;", subset, fixed=TRUE)))
})

test_that("M3 recovery summary does not forward numeric probs", {

  fake_summary <- function(variables = NULL, ...) {
    dots <- list(...)
    expect_false("probs" %in% names(dots))

    data.frame(
      variable = c("theta[1]", "theta[2]"),
      mean = c(0, 1),
      median = c(0, 1),
      sd = c(.5, .5),
      mad = c(.5, .5),
      q5 = c(-.8, .2),
      q95 = c(.8, 1.8),
      rhat = c(1.001, 1.002),
      ess_bulk = c(500, 510),
      ess_tail = c(450, 460),
      stringsAsFactors = FALSE
    )
  }

  fake_draws <- function(variables = NULL, format = NULL) {
    expect_identical(variables, "theta")
    expect_identical(format, "draws_matrix")

    cbind(
      `theta[1]` = seq(-1, 1, length.out = 101),
      `theta[2]` = seq(0, 2, length.out = 101)
    )
  }

  fake_fit <- list(
    fit = list(
      summary = fake_summary,
      draws = fake_draws
    )
  )

  out <- .ep10_m3_draw_summary_vector(
    fake_fit,
    "theta"
  )

  expect_equal(nrow(out), 2L)

  expect_true(
    all(
      c(
        "variable",
        "estimate",
        "sd",
        "lower",
        "upper",
        "rhat",
        "ess_bulk"
      ) %in% names(out)
    )
  )

  expect_equal(
    out$estimate,
    c(0, 1),
    tolerance = 1e-12
  )

  expect_true(
    all(out$lower <= out$estimate)
  )

  expect_true(
    all(out$estimate <= out$upper)
  )

  expect_equal(
    out$rhat,
    c(1.001, 1.002)
  )

  expect_equal(
    out$ess_bulk,
    c(500, 510)
  )
})
