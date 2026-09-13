# Audit rmse

Audit rmse

## Usage

``` r
audit_rmse(results, threshold = 0.3, by = c("scenario", "engine", "parameter"))
```

## Arguments

- results:

  Validation or model results.

- threshold:

  Decision or diagnostic threshold.

- by:

  Grouping variables used when summarizing results.

## Value

An R object containing rmse. The concrete class and structure follow the
selected method, engine, or input object and are preserved as documented
by that workflow.
