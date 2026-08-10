# Estimate RMS successive-sample gaze imprecision

Estimate RMS successive-sample gaze imprecision

## Usage

``` r
gaze_precision_rms_s2s(
  data,
  x = "gaze_x",
  y = "gaze_y",
  time = NULL,
  by = NULL
)
```

## Arguments

- data:

  Gaze samples.

- x, y:

  Gaze-coordinate columns.

- time:

  Optional timestamp column used to order samples.

- by:

  Optional grouping columns.
