# Template job definitions for run_eyeprocess_validation_program().
# Edit the scenarios, priors, extractors, and thresholds before substantive use.

simulate_lm <- function(n = 200L, beta = 0.5) {
  x <- stats::rnorm(n)
  y <- beta * x + stats::rnorm(n)
  list(data = data.frame(x = x, y = y), truth = c(beta = beta))
}

fit_lm <- function(simulation) stats::lm(y ~ x, data = simulation$data)

extract_lm <- function(fit) {
  co <- stats::coef(summary(fit))["x", ]
  data.frame(
    parameter = "beta",
    estimate = unname(co[[1L]]),
    std_error = unname(co[[2L]]),
    lower = unname(co[[1L]] - 1.96 * co[[2L]]),
    upper = unname(co[[1L]] + 1.96 * co[[2L]])
  )
}

model_jobs <- list(
  recovery_example = list(
    simulator = simulate_lm,
    fitter = fit_lm,
    extractor = extract_lm,
    truth_extractor = function(simulation) simulation$truth,
    grid = expand.grid(n = c(200L, 500L), beta = c(0, 0.25, 0.5)),
    spec = model_validation_spec(replications = 500L),
    seed = 20260804L
  )
)


# Example validation job for a declared process effect. This uses a simple
# recovery model as a smoke test; replace it with the model-specific fitter and
# extractor before making claims about an advanced estimator.
fit_gaze_effect <- function(simulation) {
  stats::glm(
    score ~ gaze_1 + factor(participant_id) + factor(item_id),
    data = simulation$trials,
    family = stats::binomial()
  )
}

extract_gaze_effect <- function(fit) {
  co <- stats::coef(summary(fit))["gaze_1", ]
  data.frame(
    parameter = "gaze_effect",
    estimate = unname(co[[1L]]),
    std_error = unname(co[[2L]]),
    lower = unname(co[[1L]] - 1.96 * co[[2L]]),
    upper = unname(co[[1L]] + 1.96 * co[[2L]])
  )
}

advanced_grid <- advanced_validation_grid(quick = TRUE)
advanced_grid <- advanced_grid[
  advanced_grid$missing_process == 0 &
    advanced_grid$state_misclassification == 0,
  , drop = FALSE
]

model_jobs$fit_process_irt <- list(
  simulator = simulate_advanced_process_data,
  fitter = fit_gaze_effect,
  extractor = extract_gaze_effect,
  truth_extractor = function(simulation) c(gaze_effect = simulation$truth$gaze_effect),
  grid = advanced_grid,
  spec = model_validation_spec(replications = 100L),
  seed = 20260804L
)


benchmark_jobs <- list(
  model_data = function() model_data(validation_dataset, include_features = TRUE),
  canonical_validation = function() validate_eye_dataset(validation_dataset)
)

# Supply these only when the exact engines/materials are available.
sbc_jobs <- list()
engine_jobs <- list()
reproduction_jobs <- list()
multiverse_jobs <- list()

validation_jobs <- list(
  model_jobs = model_jobs,
  sbc_jobs = sbc_jobs,
  engine_jobs = engine_jobs,
  reproduction_jobs = reproduction_jobs,
  multiverse_jobs = multiverse_jobs,
  benchmark_jobs = benchmark_jobs
)

# Grouped and leakage jobs are named after the model function when they should
# be merged automatically into that model's promotion evidence record.
grouped_jobs <- list()
leakage_jobs <- list()
advanced_evidence <- list()
evidence_spec <- advanced_model_evidence_spec()

validation_jobs$grouped_jobs <- grouped_jobs
validation_jobs$leakage_jobs <- leakage_jobs
validation_jobs$advanced_evidence <- advanced_evidence
validation_jobs$evidence_spec <- evidence_spec
