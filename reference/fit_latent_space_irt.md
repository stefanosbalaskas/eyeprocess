# Fit a latent-space IRT model using LSMjml

Fit a latent-space IRT model using LSMjml

## Usage

``` r
fit_latent_space_irt(
  response_matrix,
  dimensions = 2L,
  penalty = NULL,
  constraint = NULL,
  starts = NULL,
  tol = 0.001,
  silent = TRUE
)
```

## Arguments

- response_matrix:

  Person-by-item matrix with lowest score coded zero.

- dimensions:

  Latent-space dimensionality.

- penalty:

  Optional L2 penalty passed to \`LSMjml::LSMfit()\`.

- constraint:

  Optional norm constraint \`C\` passed to \`LSMfit()\`.

- starts:

  Starting-value strategy.

- tol:

  Numerical convergence tolerance.

- silent:

  Whether engine messages are suppressed.

## Value

An object of class "eye_latent_space_irt", stored as a named list, with
components "model", "person_coordinates", "item_coordinates",
"person_intercept", "item_intercept", "dimensions", "engine", "status".
It contains a latent-space IRT model using LSMjml and associated
metadata or diagnostics needed to interpret the result.
