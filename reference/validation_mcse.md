# Monte Carlo standard errors for validation metrics

Monte Carlo standard errors for validation metrics

## Usage

``` r
validation_mcse(results, metric = c("bias", "rmse", "coverage"))
```

## Arguments

- results:

  Validation or model results.

- metric:

  Metric to calculate or audit.

## Value

A data frame containing monte Carlo standard errors for validation
metrics. Rows represent the analysis units and columns contain the
identifiers, estimates, or diagnostics defined by the function.
