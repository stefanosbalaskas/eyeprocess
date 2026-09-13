# Create an IRT recovery design

Create an IRT recovery design

## Usage

``` r
eyeprocess_irt_recovery_design(
  sample_size = c(250L, 750L),
  n_items = c(12L, 24L),
  missing_rate = c(0, 0.15),
  testlet_sd = c(0, 0.35),
  replications = 10L,
  seed = 20260811L
)
```

## Arguments

- sample_size:

  Validation sample size or vector of sample sizes.

- n_items:

  Number of items.

- missing_rate:

  Proportion of responses or observations set missing.

- testlet_sd:

  Standard deviation of simulated testlet effects.

- replications:

  Number of simulation or validation replications.

- seed:

  Random-number seed for reproducible execution.

## Value

An object of class "eye_irt_recovery_design", "data.frame", stored as a
data frame, containing an IRT recovery design and associated metadata
needed to interpret the result.
