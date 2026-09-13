# Run a named equateIRT linking/equating function without fallback substitution

The caller supplies an exported equateIRT function name and its
arguments. eyeprocess does not replace the requested equating estimator
when the engine is unavailable.

## Usage

``` r
run_eyeprocess_equateirt(function_name, ..., engine = "equateIRT")
```

## Arguments

- function_name:

  Name of the external equateIRT function to call.

- ...:

  Additional arguments passed to the selected method or external engine.

- engine:

  Requested estimation or analysis engine.

## Value

An object of class "eye_external_irt_fit", stored as a named list, with
components "status", "engine", "function_name", "fit", "call". It
contains a named equateIRT linking/equating function without fallback
substitution and associated metadata or diagnostics needed to interpret
the result.
