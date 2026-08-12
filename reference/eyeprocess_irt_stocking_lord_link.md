# Stocking-Lord characteristic-curve linking

Stocking-Lord characteristic-curve linking

## Usage

``` r
eyeprocess_irt_stocking_lord_link(
  reference,
  focal,
  anchors = NULL,
  theta = seq(-4, 4, length.out = 81),
  weights = NULL,
  start = c(A = 1, B = 0)
)
```

## Arguments

- reference:

  Reference-form or reference-group item parameters.

- focal:

  Focal-form or focal-group item parameters.

- anchors:

  Anchor-item identifiers.

- theta:

  Latent-trait value or vector of latent-trait values.

- weights:

  Optional numerical weights.

- start:

  Starting values for numerical optimization.
