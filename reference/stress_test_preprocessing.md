# Stress test preprocessing

Stress test preprocessing

## Usage

``` r
stress_test_preprocessing(
  runner,
  variants,
  replications = 25L,
  seed = 20260808L
)
```

## Arguments

- runner:

  Function that executes one stress-test scenario.

- variants:

  Preprocessing variants to evaluate.

- replications:

  Number of simulation or validation replications.

- seed:

  Random-number seed.

## Value

An object of class "eye_irt_stress_test", "data.frame", stored as a data
frame, containing stress test preprocessing and associated metadata
needed to interpret the result.
