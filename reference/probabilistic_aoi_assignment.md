# Probabilistic AOI assignment under empirical calibration uncertainty

Probabilistic AOI assignment under empirical calibration uncertainty

## Usage

``` r
probabilistic_aoi_assignment(
  data,
  aois,
  model,
  x = "gaze_x",
  y = "gaze_y",
  draws = 500L,
  seed = 1L,
  min_probability = 0.5
)
```

## Arguments

- data:

  Gaze samples.

- aois:

  Rectangular AOI table.

- model:

  Calibration error model.

- x, y:

  Gaze-coordinate columns.

- draws:

  Monte Carlo draws.

- seed:

  Seed.

- min_probability:

  Minimum probability for assignment; lower maxima become \`NA\`.
