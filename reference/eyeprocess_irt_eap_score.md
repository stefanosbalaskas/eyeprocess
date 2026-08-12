# EAP score for dichotomous IRT item parameters

EAP score for dichotomous IRT item parameters

## Usage

``` r
eyeprocess_irt_eap_score(
  response,
  items,
  theta_grid = seq(-4, 4, length.out = 81),
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

- theta_grid:

  Grid of latent-trait values used for numerical scoring or integration.

- prior_mean:

  Mean of the normal latent-trait prior.

- prior_sd:

  Standard deviation of the normal latent-trait prior.

- D:

  Logistic scaling constant.
