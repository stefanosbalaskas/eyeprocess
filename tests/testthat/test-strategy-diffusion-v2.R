test_that("strategy signatures are prespecified and normalized", {
  spec <- theory_strategy_spec(list(analytic = c(prompt = 1, evidence = 2), heuristic = c(prompt = -1, evidence = .2)), engine = "em", multiple_starts = 2L)
  expect_s3_class(spec, "eye_theory_strategy_spec")
  expect_equal(unname(sqrt(rowSums(spec$signatures^2))), c(1, 1), tolerance = 1e-8)
  expect_error(theory_strategy_spec(list(a = c(x = 0), b = c(x = 1))), "non-zero")
})

test_that("strategy EM returns anchored posterior probabilities", {
  signatures <- rbind(analytic = c(prompt = 1, evidence = 1), heuristic = c(prompt = -1, evidence = .2))
  sim <- simulate_strategy_mixture_data(10L, 4L, signatures, seed = 12L)
  spec <- theory_strategy_spec(list(analytic = signatures[1,], heuristic = signatures[2,]), engine = "em", multiple_starts = 2L)
  fit <- fit_theory_strategy_irt(sim, spec, seed = 5L, max_iter = 30L)
  expect_s3_class(fit, "eye_theory_strategy_irt")
  probability <- strategy_posterior_probabilities(fit)
  expect_equal(nrow(probability), nrow(sim))
  expect_equal(rowSums(probability[spec$strategies]), rep(1, nrow(sim)), tolerance = 1e-6)
  uncertainty <- strategy_classification_uncertainty(fit)
  expect_true(uncertainty$summary$uncertain_fraction >= 0)
})

test_that("gaze diffusion data enforce seconds and confirmatory mappings", {
  sim <- simulate_gaze_diffusion_data(8L, 4L, seed = 9L)
  spec <- gaze_diffusion_spec(drift_features = "gaze_balance", engine = "baseline")
  prepared <- prepare_gaze_diffusion_data(sim, spec)
  expect_s3_class(prepared, "eye_gaze_diffusion_data")
  expect_equal(length(prepared$y), nrow(sim))
  expect_error(gaze_diffusion_spec(drift_features = "x", boundary_features = "x"), "only one")
})

test_that("baseline diffusion fit preserves joint data and diagnostics", {
  sim <- simulate_gaze_diffusion_data(10L, 5L, seed = 2L)
  fit <- fit_gaze_diffusion_irt(sim, gaze_diffusion_spec(drift_features = "gaze_balance", engine = "baseline"))
  expect_s3_class(fit, "eye_gaze_diffusion_irt")
  parameters <- extract_diffusion_parameters(fit)
  expect_true(all(c("component", "term", "estimate") %in% names(parameters)))
  diagnostic <- diffusion_parameter_diagnostics(fit)
  expect_equal(diagnostic$engine, "baseline")
})

test_that("advanced Stan programs are bundled", {
  expect_true(file.exists(system.file("stan", "theory_strategy_mixture.stan", package = "eyeprocess")))
  expect_true(file.exists(system.file("stan", "gaze_diffusion_irt.stan", package = "eyeprocess")))
})

test_that("Wiener censoring helpers use supported Stan call syntax", {
  stan_file <- system.file("stan", "gaze_diffusion_irt.stan", package = "eyeprocess")
  expect_true(file.exists(stan_file))
  stan_code <- paste(readLines(stan_file, warn = FALSE), collapse = "\n")

  expect_false(grepl(
    "wiener_lcdf_unnorm\\s*\\([^\\n]*\\|",
    stan_code,
    perl = TRUE
  ))
  expect_false(grepl(
    "wiener_lccdf_unnorm\\s*\\([^\\n]*\\|",
    stan_code,
    perl = TRUE
  ))
  expect_match(
    stan_code,
    "wiener_lcdf_unnorm\\(rt, boundary, nondecision, starting, drift\\)",
    perl = TRUE
  )
  expect_match(
    stan_code,
    "wiener_lccdf_unnorm\\(rt, boundary, nondecision, starting, drift\\)",
    perl = TRUE
  )
})

test_that("user-defined Wiener probability wrappers use conditional notation", {
  stan_file <- system.file("stan", "gaze_diffusion_irt.stan", package = "eyeprocess")
  expect_true(file.exists(stan_file))
  stan_code <- paste(readLines(stan_file, warn = FALSE), collapse = "\n")

  expect_equal(
    lengths(regmatches(
      stan_code,
      gregexpr(
        "selected_wiener_lpdf\\s*\\(rt\\[n\\]\\s*\\|\\s*y\\[n\\]",
        stan_code,
        perl = TRUE
      )
    )),
    2L
  )
  expect_equal(
    lengths(regmatches(
      stan_code,
      gregexpr(
        "selected_wiener_lcdf\\s*\\(rt\\[n\\]\\s*\\|\\s*y\\[n\\]",
        stan_code,
        perl = TRUE
      )
    )),
    2L
  )
  expect_equal(
    lengths(regmatches(
      stan_code,
      gregexpr(
        "selected_wiener_lccdf\\s*\\(rt\\[n\\]\\s*\\|\\s*y\\[n\\]",
        stan_code,
        perl = TRUE
      )
    )),
    2L
  )

  expect_false(grepl(
    "selected_wiener_lpdf\\s*\\(rt\\[n\\]\\s*,",
    stan_code,
    perl = TRUE
  ))
  expect_false(grepl(
    "selected_wiener_lcdf\\s*\\(rt\\[n\\]\\s*,",
    stan_code,
    perl = TRUE
  ))
  expect_false(grepl(
    "selected_wiener_lccdf\\s*\\(rt\\[n\\]\\s*,",
    stan_code,
    perl = TRUE
  ))
})

test_that("Stan generated quantities use supported absolute-value syntax", {
  stan_file <- system.file("stan", "gaze_diffusion_irt.stan", package = "eyeprocess")
  expect_true(file.exists(stan_file))
  stan_code <- paste(readLines(stan_file, warn = FALSE), collapse = "\n")

  expect_false(grepl("\\bfabs\\s*\\(", stan_code, perl = TRUE))
  expect_match(stan_code, "fmax\\(abs\\(drift\\), 0\\.25\\)", perl = TRUE)
})

