test_that("0.9 SBC and resolution audits are descriptive and deterministic", {
  expect_equal(simulation_rank_statistic(0, c(-1, 1)), 1L)
  sbc <- sbc_rank_diagnostics(rep(0:9, 10), n_draws = 9, bins = 10)
  expect_s3_class(sbc, "eye_sbc_diagnostics")
  expect_true(is.finite(sbc_ecdf_deviation(sbc)))
  sbc_uneven <- sbc_rank_diagnostics(rep(0:9, 6), n_draws = 9, bins = 6)
  expect_equal(sum(sbc_uneven$expected_count), length(sbc_uneven$ranks))
  expect_error(sbc_rank_diagnostics(0:2, n_draws = 2, bins = 1), "bins")
  g <- analysis_resolution_guard(100, 60, spatial_feature_size=.2, radial_error=.04)
  expect_s3_class(g, "eye_analysis_resolution_guard")
  expect_true(g$temporal_ok)
  ord <- audit_pupil_preprocessing_order(c("blink interpolation", "artifact filter", "baseline correction"))
  expect_equal(ord$status, "pass")
  pd <- data.frame(person_id = rep(1:2, each = 8), trial_id = rep(rep(1:2, each = 4), 2),
                   time_ms = rep(c(-300, -100, 100, 300), 4), pupil = rep(c(3.0, 3.1, 3.2, 3.3), 4))
  bs <- pupil_baseline_sensitivity(pd, by = c("person_id", "trial_id"),
                                   windows = list(W300 = c(-300, 0), W150 = c(-150, 0)))
  expect_s3_class(bs, "eye_pupil_baseline_sensitivity")
  expect_equal(nrow(bs), 8)
  pd0 <- pd; pd0$pupil[pd0$time_ms < 0] <- 0
  bs0 <- pupil_baseline_sensitivity(pd0, by = c("person_id", "trial_id"), windows = list(W = c(-300, 0)), correction = "divisive")
  expect_true(all(is.na(bs0$corrected_post_mean)))
})

test_that("0.9 calibration extras handle degenerate evidence explicitly", {
  cc <- coverage_calibration_curve(c(NA_real_, NA_real_), c(0, 0), c(1, 1), nominal = .95)
  expect_true(is.na(cc$empirical))
  expect_error(simulation_rank_statistic(0, c(0, 1), seed = -2), "seed")
  expect_error(audit_pupil_preprocessing_order(character()), "steps")
  b <- measurement_error_budget(accuracy = c(1, 2), precision = 3)
  expect_equal(nrow(b), 5)
  expect_equal(b$value[[1L]], 1)
})
