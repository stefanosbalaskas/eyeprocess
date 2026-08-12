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
