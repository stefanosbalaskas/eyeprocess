# Differential item functioning effect curve from two parameter sets

Differential item functioning effect curve from two parameter sets

## Usage

``` r
eyeprocess_irt_dif_effect_curve(
  reference_item,
  focal_item,
  theta = seq(-4, 4, length.out = 81)
)
```

## Arguments

- reference_item:

  Reference-group item parameters.

- focal_item:

  Focal-group item parameters.

- theta:

  Latent-trait value or vector of latent-trait values.
