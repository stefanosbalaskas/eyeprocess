# Compare a validation result with a frozen reference

Compare a validation result with a frozen reference

## Usage

``` r
validate_against_reference(x, reference, tolerance = 1e-06)
```

## Arguments

- x:

  Validation result.

- reference:

  Frozen reference object or RDS path.

- tolerance:

  Numeric tolerance for matched summary values.

## Value

An object of class "eye_validation_reference_comparison", stored as a
named list, with components "table", "tolerance", "pass",
"reference_hash", "current_hash". It contains a validation result with a
frozen reference and associated metadata or diagnostics needed to
interpret the result.
