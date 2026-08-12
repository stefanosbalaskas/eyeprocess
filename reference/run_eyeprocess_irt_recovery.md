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
