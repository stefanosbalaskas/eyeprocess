# Audit how well item information targets a theta distribution

Audit how well item information targets a theta distribution

## Usage

``` r
eyeprocess_irt_information_targeting(items, theta, weights = NULL, D = 1)
```

## Arguments

- items:

  Item-parameter data frame or item collection.

- theta:

  Latent-trait value or vector of latent-trait values.

- weights:

  Optional numerical weights.

- D:

  Logistic scaling constant.

## Value

An object of class "eye_irt_information_targeting", stored as a named
list, with components "weighted_information", "weighted_sem", "curve",
"weights". It contains how well item information targets a theta
distribution and associated metadata or diagnostics needed to interpret
the result.
