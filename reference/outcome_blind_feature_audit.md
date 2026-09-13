# Audit whether candidate predictors can be constructed without an outcome column

Audit whether candidate predictors can be constructed without an outcome
column

## Usage

``` r
outcome_blind_feature_audit(data, outcome, feature_fun)
```

## Arguments

- data:

  Data frame.

- outcome:

  Outcome column name.

- feature_fun:

  Function receiving outcome-hidden data and returning features.

## Value

A named list with components "status", "outcome", "feature_result",
"error", "warnings", "input_columns", "interpretation", containing
whether candidate predictors can be constructed without an outcome
column and associated metadata or diagnostics.
