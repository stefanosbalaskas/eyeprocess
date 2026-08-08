# Run generic simulation-based calibration

Run generic simulation-based calibration

## Usage

``` r
run_sbc(
  simulator,
  fitter,
  posterior_draws,
  replications = 100L,
  seed = 20260808L
)
```

## Arguments

- simulator:

  Function \`simulator(replicate)\` returning \`list(data, truth)\`;
  \`truth\` must be a named numeric vector.

- fitter:

  Function \`fitter(data)\` returning a fitted object.

- posterior_draws:

  Function returning a draws matrix/data frame whose columns match names
  in \`truth\`.

- replications:

  Number of SBC replications.

- seed:

  RNG seed.
