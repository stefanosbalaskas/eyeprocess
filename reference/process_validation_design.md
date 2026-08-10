# Define an empirical process-validation design

The design is intentionally explicit. It records measurement conditions
under which recovery, uncertainty, convergence, and failure behavior
will be evaluated. It does not imply that every combination is
appropriate for every estimator.

## Usage

``` r
process_validation_design(
  n_persons = c(50L, 150L, 500L),
  n_trials = c(10L, 30L, 80L),
  missingness = c(0, 0.05, 0.15, 0.3),
  sampling_rate_hz = c(60, 120, 300, 1000),
  aoi_error = c("low", "moderate", "severe"),
  calibration_error = c(0, 0.5, 1),
  pupil_dropout = c(0, 0.1, 0.3),
  heterogeneity = c("low", "moderate"),
  model_misspecification = c(FALSE, TRUE),
  replications = 100L,
  seed = 1L,
  label = "process_validation"
)
```

## Arguments

- n_persons:

  Participant counts.

- n_trials:

  Trial/item counts per participant.

- missingness:

  Proportion of generic process observations made missing.

- sampling_rate_hz:

  Nominal sampling rates.

- aoi_error:

  AOI uncertainty regimes.

- calibration_error:

  Calibration-error regimes in user-defined units.

- pupil_dropout:

  Pupil-specific dropout proportions.

- heterogeneity:

  Participant-heterogeneity regimes.

- model_misspecification:

  Logical regimes indicating deliberate mismatch.

- replications:

  Monte Carlo replications per condition.

- seed:

  Master simulation seed.

- label:

  Optional design label.

## Value

An \`eye_process_validation_design\` object.
