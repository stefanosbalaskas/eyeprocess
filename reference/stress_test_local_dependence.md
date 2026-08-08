# Stress test local dependence

Stress test local dependence

## Usage

``` r
stress_test_local_dependence(
  runner,
  strengths = c(0, 0.2, 0.5, 0.8),
  replications = 50L,
  seed = 20260808L
)
```

## Arguments

- runner:

  Function that executes one stress-test scenario.

- strengths:

  Local-dependence strengths to evaluate.

- replications:

  Number of simulation or validation replications.

- seed:

  Random-number seed.
