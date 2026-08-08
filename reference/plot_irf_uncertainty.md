# Plot uncertainty for flexible item response functions

For the bundled spline-reference engine, standard errors are derived on
the logit scale from the fitted GLM and transformed to response
probabilities. Exact GPIRT engines should supply their own posterior
uncertainty summaries.

## Usage

``` r
plot_irf_uncertainty(
  object,
  item = 1L,
  theta_grid = seq(-4, 4, length.out = 101),
  level = 0.95,
  ...
)
```

## Arguments

- object:

  An \`eye_gpirt\` object.

- item:

  Item name or index.

- theta_grid:

  Trait grid.

- level:

  Pointwise confidence level for the spline-reference diagnostic.

- ...:

  Graphical arguments.
