# Fit an explicit visual-context/testlet IRT model

Fit an explicit visual-context/testlet IRT model

## Usage

``` r
fit_visual_context_irt(
  response_matrix,
  registry,
  context = NULL,
  itemtype = "2PL",
  model_dimension = "Ability",
  context_dimension = "VisualContextFactor",
  SE = FALSE
)
```

## Arguments

- response_matrix:

  Person x item response matrix.

- registry:

  \`visual_context_registry()\` object.

- context:

  Optional context identifier to model; defaults to the first shared
  context.

- itemtype:

  mirt item type.

- model_dimension:

  Name for the primary latent dimension.

- context_dimension:

  Name for the context/testlet dimension.

- SE:

  Request standard errors from mirt.
