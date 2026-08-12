# Audit item-bank information coverage across a theta region

Audit item-bank information coverage across a theta region

## Usage

``` r
eyeprocess_irt_bank_coverage(
  items,
  theta = seq(-4, 4, length.out = 161),
  target_information = 5,
  target = c(-2, 2)
)
```

## Arguments

- items:

  Item-parameter data frame or item collection.

- theta:

  Latent-trait value or vector of latent-trait values.

- target_information:

  Target test-information level.

- target:

  Target level, distribution, or criterion.
