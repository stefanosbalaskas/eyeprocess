# Stress test speededness

Stress test speededness

## Usage

``` r
stress_test_speededness(
  runner,
  proportions = c(0, 0.1, 0.25, 0.4),
  replications = 50L,
  seed = 20260808L
)
```

## Arguments

- runner:

  Function that executes one stress-test scenario.

- proportions:

  Speededness proportions to evaluate.

- replications:

  Number of simulation or validation replications.

- seed:

  Random-number seed.
