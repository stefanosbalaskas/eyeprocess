# Audit stability of pupil frequency features across window lengths

Audit stability of pupil frequency features across window lengths

## Usage

``` r
audit_pupil_frequency_stability(
  data,
  windows_ms = c(500, 1000, 2000),
  by = c("person_id", "trial_id"),
  time = "time_ms",
  pupil = "pupil_bc",
  sampling_rate_hz = 60
)
```

## Arguments

- data:

  Sample-level data.

- windows_ms:

  Window lengths to evaluate.

- by, time, pupil, sampling_rate_hz:

  Passed through to feature construction.
