# Audit empirical identifiability from replicate estimates

Flags parameters with near-zero estimate variance, explosive dispersion,
excessive missingness, or strongly correlated estimates when a
covariance matrix is supplied.

## Usage

``` r
audit_identifiability(
  results,
  max_missing = 0.05,
  max_sd_ratio = 10,
  correlation_matrix = NULL,
  max_abs_correlation = 0.995
)
```

## Arguments

- results:

  Validation or model results.

- max_missing:

  Maximum acceptable missingness.

- max_sd_ratio:

  Maximum acceptable standard-deviation ratio.

- correlation_matrix:

  Optional parameter-correlation matrix.

- max_abs_correlation:

  Maximum acceptable absolute parameter correlation.
