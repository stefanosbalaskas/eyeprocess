# Compute item residual fit summaries from observed and predicted probabilities

Compute item residual fit summaries from observed and predicted
probabilities

## Usage

``` r
eyeprocess_irt_item_fit_residuals(
  responses,
  probabilities,
  item_ids = colnames(responses)
)
```

## Arguments

- responses:

  Response matrix or response data.

- probabilities:

  Probability matrix or vector, with dimensions appropriate to the
  model.

- item_ids:

  Optional item identifiers.
