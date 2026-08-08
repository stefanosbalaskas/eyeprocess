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
