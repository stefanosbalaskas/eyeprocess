# Run a mirtCAT adaptive-testing workflow without fallback substitution

Run a mirtCAT adaptive-testing workflow without fallback substitution

## Usage

``` r
run_eyeprocess_mirtcat(..., engine = "mirtCAT")
```

## Arguments

- ...:

  Additional arguments passed to the selected method or external engine.

- engine:

  Requested estimation or analysis engine.

## Value

An object of class "eye_external_irt_fit", stored as a named list, with
components "status", "engine", "fit", "call". It contains a mirtCAT
adaptive-testing workflow without fallback substitution and associated
metadata or diagnostics needed to interpret the result.
