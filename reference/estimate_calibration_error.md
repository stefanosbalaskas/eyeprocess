# Estimate empirical calibration/validation error

Estimate empirical calibration/validation error

## Usage

``` r
estimate_calibration_error(
  data,
  gaze_x = "gaze_x",
  gaze_y = "gaze_y",
  target_x = "target_x",
  target_y = "target_y",
  by = NULL
)
```

## Arguments

- data:

  Validation-target data.

- gaze_x, gaze_y:

  Recorded gaze-coordinate columns.

- target_x, target_y:

  Known target-coordinate columns.

- by:

  Optional grouping columns such as participant/session.
