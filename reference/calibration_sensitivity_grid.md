# Sensitivity grid for deterministic calibration offsets

Sensitivity grid for deterministic calibration offsets

## Usage

``` r
calibration_sensitivity_grid(
  offset_x = c(-0.02, 0, 0.02),
  offset_y = c(-0.02, 0, 0.02)
)
```

## Arguments

- offset_x, offset_y:

  Candidate offsets in coordinate units.

## Value

A tabular R object containing sensitivity grid for deterministic
calibration offsets; rows represent analysis units and columns contain
the returned quantities.
