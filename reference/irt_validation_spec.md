# Specify a validation programme for a process-IRT model

Creates an evidence contract rather than executing a particular
estimator. The object can be passed to stress-test and evidence-grading
helpers.

## Usage

``` r
irt_validation_spec(
  model_id,
  replications = 250L,
  parameters = NULL,
  metrics = c("bias", "rmse", "coverage", "interval_width", "convergence"),
  grouped_validation = c("device", "session", "site"),
  preprocessing_variants = NULL,
  misspecification_scenarios = NULL,
  thresholds = list(max_abs_bias = 0.1, max_rmse = 0.3, min_coverage = 0.9,
    max_failure_rate = 0.05, min_external_folds = 2L),
  seed = 20260808L,
  notes = NULL
)
```

## Arguments

- model_id:

  Stable model identifier.

- replications:

  Number of simulation replications planned.

- parameters:

  Parameter families expected to be recovered.

- metrics:

  Recovery metrics to require.

- grouped_validation:

  Grouping variables for transport validation.

- preprocessing_variants:

  Optional named preprocessing variants.

- misspecification_scenarios:

  Optional named scenarios.

- thresholds:

  Named evidence thresholds.

- seed:

  Reproducibility seed.

- notes:

  Free-text scientific notes.
