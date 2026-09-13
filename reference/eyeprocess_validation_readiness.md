# Evaluate readiness of a Milestone \#2 validation evidence bundle

Evaluate readiness of a Milestone \#2 validation evidence bundle

## Usage

``` r
eyeprocess_validation_readiness(
  x,
  required = c("design", "recovery", "stress", "reliability", "negative_controls",
    "claims", "provenance")
)
```

## Arguments

- x:

  Object to validate, summarize, verify, or otherwise process.

- required:

  Required evidence components or requirements.

## Value

An object of class "eye_validation_readiness", stored as a named list,
with components "ready", "table", "hash_valid", "source_commit". It
contains readiness of a Milestone \#2 validation evidence bundle and
associated metadata or diagnostics needed to interpret the result.
