# Stress test missingness

Stress test missingness

## Usage

``` r
stress_test_missingness(
  runner,
  mechanisms = c("MCAR", "MAR", "MNAR_omission", "not_reached"),
  rates = c(0.05, 0.15, 0.3),
  replications = 50L,
  seed = 20260808L
)
```

## Arguments

- runner:

  Function that executes one stress-test scenario.

- mechanisms:

  Missingness mechanisms to evaluate.

- rates:

  Missingness rates to evaluate.

- replications:

  Number of simulation or validation replications.

- seed:

  Random-number seed.

## Value

An object of class "eye_irt_stress_test", "data.frame", stored as a data
frame, containing stress test missingness and associated metadata needed
to interpret the result.
