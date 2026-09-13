# Freeze a compact validation reference for regression testing

Freeze a compact validation reference for regression testing

## Usage

``` r
freeze_validation_reference(x, path = NULL, digits = 8L)
```

## Arguments

- x:

  Validation result.

- path:

  Optional RDS path.

- digits:

  Numeric rounding applied before hashing.

## Value

An object of class "eye_validation_reference", stored as a named list,
with components "summary", "failure_profile", "design_hash",
"summary_hash", "created_at", "status". It contains freeze a compact
validation reference for regression testing and associated metadata or
diagnostics needed to interpret the result.
