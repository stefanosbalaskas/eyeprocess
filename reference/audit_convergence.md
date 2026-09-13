# Audit convergence and classified failures

Audit convergence and classified failures

## Usage

``` r
audit_convergence(results, minimum = 0.95, by = c("scenario", "engine"))
```

## Arguments

- results:

  Validation or model results.

- minimum:

  Minimum acceptable value or threshold.

- by:

  Grouping variables used when summarizing results.

## Value

An object of class "eye_irt_convergence_audit", "data.frame", stored as
a data frame, containing convergence and classified failures and
associated metadata needed to interpret the result.
