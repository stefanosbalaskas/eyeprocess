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

## Value

An R object containing propagate empirical calibration uncertainty
around gaze samples. The concrete class and structure follow the
selected method, engine, or input object and are preserved as documented
by that workflow.
