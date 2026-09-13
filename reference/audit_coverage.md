# Audit coverage

Audit coverage

## Usage

``` r
audit_coverage(
  results,
  minimum = 0.9,
  maximum = 1,
  by = c("scenario", "engine", "parameter")
)
```

## Arguments

- results:

  Validation or model results.

- minimum:

  Minimum acceptable value or threshold.

- maximum:

  Maximum acceptable value or threshold.

- by:

  Grouping variables used when summarizing results.

## Value

An R object containing coverage. The concrete class and structure follow
the selected method, engine, or input object and are preserved as
documented by that workflow.
