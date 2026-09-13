# Apply a conservative software-release evidence gate

Apply a conservative software-release evidence gate

## Usage

``` r
eyeprocess_validation_release_gate(
  readiness,
  acceptance = NULL,
  require_hash = TRUE
)
```

## Arguments

- readiness:

  Validation-readiness result.

- acceptance:

  Acceptance-rule results.

- require_hash:

  Whether a verified integrity hash is required.

## Value

An object of class "eye_validation_release_gate", stored as a named
list, with components "pass", "readiness", "acceptance", "hash",
"interpretation". It contains a conservative software-release evidence
gate and associated metadata or diagnostics needed to interpret the
result.
