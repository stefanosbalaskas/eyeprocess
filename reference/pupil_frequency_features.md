# Extract pupil frequency-domain and activity features by group

Extract pupil frequency-domain and activity features by group

## Usage

``` r
pupil_frequency_features(
  data,
  by = c("person_id", "trial_id"),
  time = "time_ms",
  pupil = "pupil_bc",
  sampling_rate_hz = 60,
  low_band = c(0.05, 0.5),
  high_band = c(0.5, 4)
)
```

## Arguments

- data:

  Sample-level data.

- by:

  Grouping columns, e.g. person and trial/window.

- time, pupil:

  Column names.

- sampling_rate_hz:

  Either a scalar or a column name.

- low_band, high_band:

  Frequency bands.
