# Run IRT parameter recovery with the exact mirt engine

When mirt is unavailable the function returns a gated result rather than
a substitute estimator.

## Usage

``` r
run_eyeprocess_irt_recovery(design, engine = "mirt", verbose = TRUE)
```

## Arguments

- design:

  Validation or simulation design object.

- engine:

  Requested estimation or analysis engine.

- verbose:

  Value supplied for the verbose argument.

## Value

An object of class "eye_irt_recovery_result", stored as a named list,
with components "design", "estimates", "failures", "engine". It contains
iRT parameter recovery with the exact mirt engine and associated
metadata or diagnostics needed to interpret the result.
