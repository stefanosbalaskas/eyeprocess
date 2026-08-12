# Declare the Milestone \#2 measurement-quality stress evidence plan

Declare the Milestone \#2 measurement-quality stress evidence plan

## Usage

``` r
eyeprocess_stress_evidence_plan(
  missing_gaze = c(0, 0.05, 0.15, 0.3),
  pupil_dropout = c(0, 0.05, 0.15),
  calibration_offset = c(0, 0.01, 0.03, 0.06),
  sampling_jitter = c(0, 0.05, 0.15),
  aoi_label_noise = c(0, 0.02, 0.1),
  device_shift = c(0, 0.02, 0.05),
  trial_imbalance = c(0, 0.1, 0.25),
  seed = 20260811L
)
```

## Arguments

- missing_gaze:

  Severity level for synthetic gaze missingness.

- pupil_dropout:

  Severity level for synthetic pupil dropout.

- calibration_offset:

  Magnitude of synthetic calibration offset.

- sampling_jitter:

  Magnitude of synthetic sampling-time jitter.

- aoi_label_noise:

  Rate of synthetic AOI-label corruption.

- device_shift:

  Magnitude of synthetic device shift.

- trial_imbalance:

  Severity of synthetic trial imbalance.

- seed:

  Random-number seed for reproducible execution.
