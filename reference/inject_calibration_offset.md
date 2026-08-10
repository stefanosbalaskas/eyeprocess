# Inject additive gaze calibration offset in coordinate units

Inject additive gaze calibration offset in coordinate units

## Usage

``` r
inject_calibration_offset(
  data,
  x = "gaze_x",
  y = "gaze_y",
  offset_x = 0,
  offset_y = 0
)
```

## Arguments

- data:

  Data.

- x, y:

  Gaze coordinate columns.

- offset_x, offset_y:

  Additive offsets in the same units as x/y.
