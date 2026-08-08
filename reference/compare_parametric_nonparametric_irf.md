# Compare conventional logistic and flexible IRF shapes

Compare conventional logistic and flexible IRF shapes

## Usage

``` r
compare_parametric_nonparametric_irf(
  response_matrix,
  gpirt_object = NULL,
  theta_grid = seq(-4, 4, length.out = 101)
)
```

## Arguments

- response_matrix:

  Person-by-item response matrix.

- gpirt_object:

  Value supplied to \`gpirt_object\`; see Details for its model-specific
  role.

- theta_grid:

  Grid of latent-trait values used for evaluation.
