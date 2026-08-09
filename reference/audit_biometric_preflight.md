# Audit incoming biometric/process data before modelling

Aggregates trial-level signal-quality indicators by person/recording (or
other supplied grouping columns) and returns review-oriented flags. No
rows are automatically deleted.

## Usage

``` r
audit_biometric_preflight(
  data,
  by = c("person_id"),
  spec = process_preflight_spec(),
  valid_gaze_prop = "valid_gaze_prop",
  valid_pupil_prop = "valid_pupil_prop",
  missing_gaze = "missing_gaze",
  missing_pupil = "missing_pupil",
  rt_ms = "rt_ms",
  blink_cluster_count = "blink_cluster_count",
  sampling_rate_hz = "sampling_rate_hz"
)
```

## Arguments

- data:

  Trial- or recording-level data.

- by:

  Grouping columns, usually participant and optionally
  recording/session.

- spec:

  A \`process_preflight_spec()\` object.

- valid_gaze_prop, valid_pupil_prop:

  Column names for validity proportions.

- missing_gaze, missing_pupil:

  Column names for missingness indicators/proportions.

- rt_ms:

  Response-time column.

- blink_cluster_count:

  Blink-cluster count column.

- sampling_rate_hz:

  Sampling-rate column.

## Value

An \`eye_biometric_preflight\` object.
