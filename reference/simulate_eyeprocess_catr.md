# Run a catR adaptive-testing simulation without fallback substitution

Run a catR adaptive-testing simulation without fallback substitution

## Usage

``` r
simulate_eyeprocess_catr(itemBank, trueTheta = 0, ..., engine = "catR")
```

## Arguments

- itemBank:

  Item bank supplied to catR.

- trueTheta:

  Known true latent-trait value used for CAT simulation.

- ...:

  Additional arguments passed to the selected method or external engine.

- engine:

  Requested estimation or analysis engine.

## Value

An object of class "eye_external_irt_fit", stored as a named list, with
components "status", "engine", "fit", "call". It contains a catR
adaptive-testing simulation without fallback substitution and associated
metadata or diagnostics needed to interpret the result.
