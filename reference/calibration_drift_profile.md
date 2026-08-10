# Calibration drift profile across sessions/batches

Calibration drift profile across sessions/batches

## Usage

``` r
calibration_drift_profile(
  data,
  by,
  gaze_x = "gaze_x",
  gaze_y = "gaze_y",
  target_x = "target_x",
  target_y = "target_y"
)
```

## Arguments

- data:

  Validation-target data.

- by:

  Ordered batch/session column.

- gaze_x, gaze_y:

  Recorded gaze-coordinate columns.

- target_x, target_y:

  Known target-coordinate columns.
