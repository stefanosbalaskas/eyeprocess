# Create a gated KDE latent-distribution IRT interface

Create a gated KDE latent-distribution IRT interface

## Usage

``` r
fit_kde_latent_distribution_irt(response_matrix, engine = NULL, ...)
```

## Arguments

- response_matrix:

  Response data supplied to an optional external engine.

- engine:

  Optional function implementing the exact/nonparametric marginal
  likelihood estimator. If omitted, a gated specification is returned.

- ...:

  Passed to \`engine\` when supplied.
