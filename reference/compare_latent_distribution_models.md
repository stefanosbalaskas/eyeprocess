# Compare simple latent-distribution reference models

Compares Gaussian, location/scale Student-t, and two-normal-mixture
reference densities by AIC/BIC. This is a stress-test diagnostic; it
does not change the latent distribution inside an already fitted IRT
model.

## Usage

``` r
compare_latent_distribution_models(theta)
```

## Arguments

- theta:

  Numeric latent-trait draws/estimates.

## Value

An object of class "eye_latent_distribution_comparison", stored as a
named list, with components "comparison", "audit", "student_t",
"mixture", "status". It contains simple latent-distribution reference
models and associated metadata or diagnostics needed to interpret the
result.
