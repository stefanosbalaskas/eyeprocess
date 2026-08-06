test_that("dynamic IRTree simulation and design preserve transitions", {
  sim <- simulate_dynamic_irtree_data(n_person = 12L, n_item = 6L, transitions_per_trial = 8L, seed = 11L)
  expect_true(all(c("participant_id", "item_id", "trial_id", "to_state", "time") %in% names(sim$transitions)))
  spec <- dynamic_irtree_spec(engine = "multinomial", condition_columns = NULL)
  prepared <- prepare_dynamic_irtree_data(sim$transitions, spec)
  design <- dynamic_transition_design(prepared, spec)
  expect_s3_class(design, "eye_transition_design")
  expect_equal(nrow(design$X), nrow(prepared))
  expect_equal(ncol(design$allowed), length(design$states))
})

test_that("structural zeros are enforced in transition masks", {
  states <- c("prompt", "option", "submit")
  mask <- structural_transition_mask(states, structural_zeros = data.frame(from = "submit", to = "prompt"))
  expect_false(mask["submit", "prompt"])
  expect_true(mask["prompt", "option"])
})

test_that("multinomial transition baseline returns probabilities", {
  sim <- simulate_dynamic_irtree_data(n_person = 15L, n_item = 5L, transitions_per_trial = 7L, seed = 4L)
  spec <- dynamic_irtree_spec(engine = "multinomial")
  fit <- fit_dynamic_irtree(sim$transitions, spec, min_transitions = 2L)
  expect_s3_class(fit, "eye_dynamic_irtree")
  probability <- decode_dynamic_states(fit, "probability")
  expect_equal(nrow(probability), nrow(fit$transitions))
  expect_equal(unname(rowSums(probability[setdiff(names(probability), "transition")])), rep(1, nrow(probability)), tolerance = 1e-6)
})

test_that("transition diagnostics and comparison are explicit", {
  sim <- simulate_dynamic_irtree_data(n_person = 10L, n_item = 4L, transitions_per_trial = 8L, seed = 20L)
  fit <- fit_dynamic_irtree(sim$transitions, dynamic_irtree_spec(engine = "multinomial"), min_transitions = 2L)
  diagnostic <- transition_residual_diagnostics(fit)
  expect_s3_class(diagnostic, "eye_transition_diagnostics")
  comparison <- compare_dynamic_transition_models(list(multinomial = fit))
  expect_true(all(c("model", "engine", "AIC", "BIC") %in% names(comparison)))
})

test_that("Stan programs are bundled but optional", {
  expect_true(file.exists(system.file("stan", "dynamic_irtree_observed.stan", package = "eyeprocess")))
  expect_true(file.exists(system.file("stan", "dynamic_irtree_hidden.stan", package = "eyeprocess")))
})
