# Audit external/structural validity of process traits

Audit external/structural validity of process traits

## Usage

``` r
audit_process_external_validity(
  data,
  criterion,
  predictors,
  baseline_predictors = NULL
)
```

## Arguments

- data:

  Person-level data containing a criterion and process predictors.

- criterion:

  External criterion column.

- predictors:

  Process predictors.

- baseline_predictors:

  Optional baseline predictors for incremental validity.

## Value

An object of class "eye_process_external_validity", stored as a named
list, with components "full_model", "baseline_model", "comparison",
"associations", "criterion", "predictors", "baseline_predictors",
"data", "incremental_r2", "status", "caveat". It contains
external/structural validity of process traits and associated metadata
or diagnostics needed to interpret the result.
