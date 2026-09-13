# Test characteristic curve for dichotomous item parameters

Test characteristic curve for dichotomous item parameters

## Usage

``` r
eyeprocess_irt_test_characteristic_curve(theta, items, D = 1)
```

## Arguments

- theta:

  Latent-trait value or vector of latent-trait values.

- items:

  Item-parameter data frame or item collection.

- D:

  Logistic scaling constant.

## Value

An object of class "eye_irt_test_characteristic_curve", "data.frame",
stored as a data frame, containing characteristic curve for dichotomous
item parameters and associated metadata needed to interpret the result.
