test_that("dynamic IRTree baseline fits supported destination states", {
  x <- simulate_eye_dataset(n_person = 8, n_item = 5, sampling_rate = 20, trial_duration = .6, seed = 201)
  fit <- fit_dynamic_irtree(x, dynamic_irtree_spec(source = "samples", include_person = FALSE), min_transitions = 5)
  expect_s3_class(fit, "eye_dynamic_irtree")
  expect_gt(nrow(fit$transitions), 0)
  expect_gt(length(fit$fits), 0)
})

test_that("functional pupil IRT derives basis features and fits a model", {
  x <- simulate_eye_dataset(n_person = 8, n_item = 5, sampling_rate = 20, trial_duration = .6, seed = 202)
  fit <- fit_joint_functional_pupil_irt(x, functional_pupil_irt_spec(df = 3, engine = "two_stage_glm"))
  expect_s3_class(fit, "eye_functional_pupil_irt")
  expect_gt(length(fit$feature_names), 0)
  expect_s3_class(fit$model, "eyeprocess_model")
})

test_that("theory-defined strategy model returns posterior probabilities", {
  sim <- simulate_advanced_process_data(n_person = 12, n_item = 5, seed = 203)
  prototypes <- rbind(strategy_1 = c(gaze_1 = -0.7, gaze_2 = 0.7), strategy_2 = c(gaze_1 = 0.7, gaze_2 = -0.7))
  spec <- theory_strategy_spec(prototypes, feature_sd = .6)
  sim$trials$gaze_1[[1L]] <- NA_real_
  fit <- fit_theory_strategy_irt(sim$trials, spec)
  expect_s3_class(fit, "eye_theory_strategy_irt")
  expect_equal(nrow(fit$posterior), nrow(sim$trials))
  expect_true(is.na(fit$assignment[[1L]]))
  usable <- stats::complete.cases(fit$posterior)
  expect_true(all(abs(rowSums(fit$posterior[usable, , drop = FALSE]) - 1) < 1e-8))
  expect_error(theory_strategy_spec(prototypes, feature_sd = 0), "positive finite")
})

test_that("gaze diffusion approximation produces item-level parameters", {
  x <- simulate_eye_dataset(n_person = 20, n_item = 8, sampling_rate = 10, trial_duration = .3, seed = 204)
  x <- derive_all_features(x)
  feature_names <- intersect(c("dwell_time_ms", "pupil_mean"), names(features_wide(x, id_cols = c("recording_id", "participant_id", "trial_id", "item_id"))))
  fit <- fit_gaze_diffusion_irt(x, gaze_diffusion_spec(engine = "ez_regression", gaze_features = feature_names))
  expect_s3_class(fit, "eye_gaze_diffusion_irt")
  expect_true(all(c("item_id", "drift_rate", "boundary_separation") %in% names(fit$data)))
})


test_that("advanced validation grid encodes the declared Monte Carlo factors", {
  grid <- advanced_validation_grid(quick = TRUE)
  expected <- c(
    "n_person", "n_item", "ability_speed_correlation", "gaze_effect",
    "feature_reliability", "missing_process", "state_misclassification",
    "pupil_ar1", "luminance_effect", "dif_effect", "local_dependence"
  )
  expect_true(all(expected %in% names(grid)))
  expect_gt(nrow(grid), 1L)
  expect_lt(nrow(grid), 30L)
  expect_true(all(grid$feature_reliability > 0 & grid$feature_reliability <= 1))
  full <- advanced_validation_grid(quick = TRUE, full_factorial = TRUE)
  expect_gt(nrow(full), nrow(grid))
})

test_that("advanced simulator returns product, timing, gaze, state, pupil, and truth layers", {
  sim <- simulate_advanced_process_data(
    n_person = 20, n_item = 8, n_time = 12, n_states = 4,
    ability_speed_correlation = 0.35, gaze_effect = 0.4,
    feature_reliability = 0.65, missing_process = 0.25,
    state_misclassification = 0.30, pupil_ar1 = 0.50,
    luminance_effect = 0.40, dif_effect = 0.30,
    local_dependence = 0.20, seed = 205
  )
  expect_true(all(c("trials", "states", "pupil", "truth") %in% names(sim)))
  expect_equal(nrow(sim$trials), 20L * 8L)
  expect_true(anyNA(sim$trials$gaze_1))
  expect_true(any(sim$states$state != sim$states$true_state))
  expect_true(all(c("score", "response_time", "gaze_1", "gaze_2", "strategy", "group", "dif_item", "testlet") %in% names(sim$trials)))
  expect_true(all(c("ability_speed_correlation", "gaze_effect", "feature_reliability", "pupil_ar1") %in% names(sim$truth)))
  expect_error(simulate_advanced_process_data(feature_reliability = 0), "outside their valid ranges")
})
