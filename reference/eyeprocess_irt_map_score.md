# MAP score for dichotomous IRT item parameters

MAP score for dichotomous IRT item parameters

## Usage

``` r
eyeprocess_irt_map_score(
  response,
  items,
  bounds = c(-6, 6),
  prior_mean = 0,
  prior_sd = 1,
  D = 1
)
```

## Arguments

- response:

  Observed item response or response variable.

- items:

  Item-parameter data frame or item collection.

- bounds:

  Numerical lower and upper optimization bounds.

- prior_mean:

  Mean of the normal latent-trait prior.

- prior_sd:

  Standard deviation of the normal latent-trait prior.

- D:

  Logistic scaling constant.
