# Compute item information for transparent IRT families

Compute item information for transparent IRT families

## Usage

``` r
eyeprocess_irt_item_information(
  theta,
  family = c("2pl", "3pl", "4pl", "grm", "gpcm", "nominal"),
  ...,
  D = 1
)
```

## Arguments

- theta:

  Latent-trait value or vector of latent-trait values.

- family:

  IRT response family or model family.

- ...:

  Additional arguments passed to the selected method or external engine.

- D:

  Logistic scaling constant.
