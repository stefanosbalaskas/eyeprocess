# Build an empirical bivariate calibration-error model

Build an empirical bivariate calibration-error model

## Usage

``` r
calibration_error_model(
  data,
  gaze_x = "gaze_x",
  gaze_y = "gaze_y",
  target_x = "target_x",
  target_y = "target_y"
)
```

## Arguments

- data:

  Validation-target data.

- gaze_x, gaze_y:

  Recorded gaze-coordinate columns.

- target_x, target_y:

  Known target-coordinate columns.

## Value

An object of class "eye_calibration_error_model", stored as a named
list, with components "mean_error", "covariance", "errors", "n",
"metrics", "coordinate_units", "status", "caveat". It contains an
empirical bivariate calibration-error model and associated metadata or
diagnostics needed to interpret the result.
