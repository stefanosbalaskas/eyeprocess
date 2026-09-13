# Empirical gaze data-quality profile

Empirical gaze data-quality profile

## Usage

``` r
gaze_data_quality_profile(
  data,
  x = "gaze_x",
  y = "gaze_y",
  time = "timestamp_ms",
  target_x = NULL,
  target_y = NULL,
  valid = NULL,
  by = NULL,
  time_unit = c("ms", "s", "us")
)
```

## Arguments

- data:

  Gaze samples.

- x, y:

  Gaze coordinates.

- time:

  Timestamp column.

- target_x, target_y:

  Optional known target coordinates.

- valid:

  Optional validity indicator column.

- by:

  Optional grouping columns.

- time_unit:

  Timestamp unit.

## Value

An object of class "eye_data_quality_profile", stored as a named list,
with components "table", "coordinate_units", "caveat". It contains
empirical gaze data-quality profile and associated metadata or
diagnostics needed to interpret the result.
