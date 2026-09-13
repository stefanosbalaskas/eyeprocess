# Audit the empirical latent-trait distribution

Audit the empirical latent-trait distribution

## Usage

``` r
audit_latent_distribution(theta, tail_z = 3)
```

## Arguments

- theta:

  Numeric latent-trait draws/estimates.

- tail_z:

  Absolute standardized threshold used for tail-rate diagnostics.

## Value

A data frame containing the empirical latent-trait distribution. Rows
represent the analysis units and columns contain the identifiers,
estimates, or diagnostics defined by the function.
