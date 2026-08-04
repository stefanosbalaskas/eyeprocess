# Run simulation-based calibration

Run simulation-based calibration

## Usage

``` r
simulation_based_calibration(
  simulator,
  fitter,
  posterior_draws,
  truth_extractor,
  replications = 100L,
  seed = 1L,
  ...
)
```

## Arguments

- simulator:

  Function returning simulated data and named truth.

- fitter:

  Function receiving one simulation result.

- posterior_draws:

  Function returning a numeric matrix/data frame whose columns are named
  parameters.

- truth_extractor:

  Function returning a named numeric truth vector.

- replications:

  Number of simulated data sets.

- seed:

  Random seed.

- ...:

  Passed to \`simulator()\`.

## Value

An \`eye_sbc\` object with parameter ranks and calibration summaries.
