# Run a generic misspecification stress-test grid

Run a generic misspecification stress-test grid

## Usage

``` r
stress_test_misspecification(
  scenarios,
  runner,
  replications = 50L,
  seed = 20260808L
)
```

## Arguments

- scenarios:

  Data frame or named list describing scenarios.

- runner:

  Function \`runner(scenario, replicate)\` returning a one-row or tidy
  data frame. Errors are retained as classified failures.

- replications:

  Number of simulation or validation replications.

- seed:

  Random-number seed.
