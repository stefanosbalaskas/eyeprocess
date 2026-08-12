# Bounded ML score for dichotomous IRT item parameters

Bounded ML score for dichotomous IRT item parameters

## Usage

``` r
eyeprocess_irt_mle_score(response, items, bounds = c(-6, 6), D = 1)
```

## Arguments

- response:

  Observed item response or response variable.

- items:

  Item-parameter data frame or item collection.

- bounds:

  Numerical lower and upper optimization bounds.

- D:

  Logistic scaling constant.
