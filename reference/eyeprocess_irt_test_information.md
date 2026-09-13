# Compute a test information curve from item parameters

Compute a test information curve from item parameters

## Usage

``` r
eyeprocess_irt_test_information(theta, items, D = 1)
```

## Arguments

- theta:

  Latent-trait value or vector of latent-trait values.

- items:

  Item-parameter data frame or item collection.

- D:

  Logistic scaling constant.

## Value

An object of class "eye_irt_information_profile", "data.frame", stored
as a data frame, containing a test information curve from item
parameters and associated metadata needed to interpret the result.
