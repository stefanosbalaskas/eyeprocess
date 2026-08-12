# Declare an eyeprocess validation-evidence plan

Creates a deterministic validation plan. The plan describes
software-validation scenarios and does not constitute evidence for the
construct validity of any gaze, pupil, response-time, or psychometric
measure.

## Usage

``` r
eyeprocess_validation_plan(
  families = c("recovery", "sbc", "stress", "reliability", "negative_control"),
  sample_size = c(250L, 750L),
  n_items = c(12L, 24L),
  missing_rate = c(0, 0.15),
  noise_level = c("reference", "elevated"),
  specification = c("correct", "misspecified"),
  replications = 20L,
  seed = 20260811L,
  label = "eyeprocess-0.9-m2"
)
```

## Arguments

- families:

  Validation-evidence families to include.

- sample_size:

  Validation sample size or vector of sample sizes.

- n_items:

  Number of items.

- missing_rate:

  Proportion of responses or observations set missing.

- noise_level:

  Declared simulation noise regime.

- specification:

  Whether the validation scenario is correctly specified or deliberately
  misspecified.

- replications:

  Number of simulation or validation replications.

- seed:

  Random-number seed for reproducible execution.

- label:

  Human-readable label.
