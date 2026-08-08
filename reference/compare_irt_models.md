# Compare multimodal IRT model objects

Uses supplied scoring functions when models expose different engines.
The default extracts AIC/BIC/logLik when available and never treats
in-sample fit as sufficient evidence for model promotion.

## Usage

``` r
compare_irt_models(..., names = NULL)
```

## Arguments

- ...:

  Additional arguments passed to the selected model, engine, or method.

- names:

  Value supplied to \`names\`; see Details for its model-specific role.
