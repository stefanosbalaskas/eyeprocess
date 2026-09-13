# Summarise measurement precision across a theta region

Summarise measurement precision across a theta region

## Usage

``` r
eyeprocess_irt_measurement_precision_profile(
  theta,
  items,
  target = c(-2, 2),
  D = 1
)
```

## Arguments

- theta:

  Latent-trait value or vector of latent-trait values.

- items:

  Item-parameter data frame or item collection.

- target:

  Target level, distribution, or criterion.

- D:

  Logistic scaling constant.

## Value

An object of class "eye_irt_precision_profile", stored as a named list,
with components "curve", "target", "area", "min_information", "max_sem".
It contains measurement precision across a theta region and associated
metadata or diagnostics needed to interpret the result.
