# Rank validation conditions by a transparent robustness score

Rank validation conditions by a transparent robustness score

## Usage

``` r
validation_condition_ranking(
  x,
  weights = c(rmse = 1, abs_bias = 1, coverage_error = 1, failure_rate = 1)
)
```

## Arguments

- x:

  Validation result.

- weights:

  Named weights for rmse, absolute bias, coverage error, and failure
  rate.
