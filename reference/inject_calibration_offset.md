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

## Value

An R object containing inject additive gaze calibration offset in
coordinate units. The concrete class and structure follow the selected
method, engine, or input object and are preserved as documented by that
workflow.
