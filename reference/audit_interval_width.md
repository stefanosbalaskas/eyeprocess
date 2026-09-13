# Audit interval width

Audit interval width

## Usage

``` r
audit_interval_width(
  results,
  maximum = Inf,
  by = c("scenario", "engine", "parameter")
)
```

## Arguments

- results:

  Validation or model results.

- maximum:

  Maximum acceptable value or threshold.

- by:

  Grouping variables used when summarizing results.

## Value

An R object containing interval width. The concrete class and structure
follow the selected method, engine, or input object and are preserved as
documented by that workflow.
