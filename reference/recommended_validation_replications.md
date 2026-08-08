# Approximate simulation replications needed for a target Monte Carlo error

Approximate simulation replications needed for a target Monte Carlo
error

## Usage

``` r
recommended_validation_replications(
  target_mcse = 0.01,
  metric = c("coverage", "mean"),
  anticipated_sd = 1,
  anticipated_probability = 0.95,
  minimum = 100L
)
```

## Arguments

- target_mcse:

  Target Monte Carlo standard error.

- metric:

  Metric to calculate or audit.

- anticipated_sd:

  Anticipated standard deviation.

- anticipated_probability:

  Anticipated probability for a binary metric.

- minimum:

  Minimum acceptable value or threshold.
