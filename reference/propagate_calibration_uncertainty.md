# Propagate empirical calibration uncertainty around gaze samples

Propagate empirical calibration uncertainty around gaze samples

## Usage

``` r
propagate_calibration_uncertainty(
  data,
  model,
  x = "gaze_x",
  y = "gaze_y",
  draws = 500L,
  seed = 1L
)
```

## Arguments

- data:

  Gaze samples.

- model:

  Calibration error model.

- x, y:

  Gaze-coordinate columns.

- draws:

  Monte Carlo draws per sample.

- seed:

  Seed.
