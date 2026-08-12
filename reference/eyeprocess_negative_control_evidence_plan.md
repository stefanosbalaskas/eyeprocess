# Declare negative-control evidence targets

Declare negative-control evidence targets

## Usage

``` r
eyeprocess_negative_control_evidence_plan(
  controls = c("permutation", "temporal_shift", "placebo_window", "known_leakage"),
  replications = 100L,
  seed = 20260811L
)
```

## Arguments

- controls:

  Negative-control methods requested by the evidence plan.

- replications:

  Number of simulation or validation replications.

- seed:

  Random-number seed for reproducible execution.
