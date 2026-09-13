# Audit monotonicity of an item response curve

Audit monotonicity of an item response curve

## Usage

``` r
eyeprocess_irt_monotonicity_audit(theta, probability, tolerance = 1e-08)
```

## Arguments

- theta:

  Latent-trait value or vector of latent-trait values.

- probability:

  Model-implied probability vector.

- tolerance:

  Numerical or decision tolerance.

## Value

An object of class "eye_irt_monotonicity_audit", stored as a named list,
with components "monotone_non_decreasing", "n_decreases",
"largest_decrease", "theta", "probability". It contains monotonicity of
an item response curve and associated metadata or diagnostics needed to
interpret the result.
