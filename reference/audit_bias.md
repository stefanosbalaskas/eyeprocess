# Audit bias

Audit bias

## Usage

``` r
audit_bias(results, threshold = 0.1, by = c("scenario", "engine", "parameter"))
```

## Arguments

- results:

  Validation or model results.

- threshold:

  Decision or diagnostic threshold.

- by:

  Grouping variables used when summarizing results.

## Value

An R object containing bias. The concrete class and structure follow the
selected method, engine, or input object and are preserved as documented
by that workflow.
