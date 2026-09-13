# Stress test latent distribution

Stress test latent distribution

## Usage

``` r
stress_test_latent_distribution(runner, replications = 50L, seed = 20260808L)
```

## Arguments

- runner:

  Function that executes one stress-test scenario.

- replications:

  Number of simulation or validation replications.

- seed:

  Random-number seed.

## Value

An object of class "eye_irt_stress_test", "data.frame", stored as a data
frame, containing stress test latent distribution and associated
metadata needed to interpret the result.
