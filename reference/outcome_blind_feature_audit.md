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
