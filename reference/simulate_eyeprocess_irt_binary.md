# Simulate dichotomous IRT responses with optional local dependence and missingness

Simulate dichotomous IRT responses with optional local dependence and
missingness

## Usage

``` r
simulate_eyeprocess_irt_binary(
  n_persons = 500L,
  items,
  theta = NULL,
  missing_rate = 0,
  testlet_sd = 0,
  seed = 1L,
  D = 1
)
```

## Arguments

- n_persons:

  Number of persons.

- items:

  Item-parameter data frame or item collection.

- theta:

  Latent-trait value or vector of latent-trait values.

- missing_rate:

  Proportion of responses or observations set missing.

- testlet_sd:

  Standard deviation of simulated testlet effects.

- seed:

  Random-number seed for reproducible execution.

- D:

  Logistic scaling constant.

## Value

An object of class "eye_irt_simulation", stored as a named list, with
components "responses", "probabilities", "theta", "items",
"missing_rate", "testlet_sd", "seed". It contains dichotomous IRT
responses with optional local dependence and missingness and associated
metadata or diagnostics needed to interpret the result.
