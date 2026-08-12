# Run simulation-based calibration for known-item IRT ability scoring

Simulates abilities from the declared normal prior, responses from the
known item-response model, and posterior draws from the same grid-based
scoring algorithm used by \`eyeprocess_irt_eap_score()\`. This validates
computational calibration of the scoring workflow under the declared
generative model; it does not establish empirical adequacy or construct
validity.

## Usage

``` r
run_eyeprocess_irt_ability_sbc(
  items,
  replications = 200L,
  posterior_draws = 99L,
  theta_grid = seq(-5, 5, length.out = 401),
  prior_mean = 0,
  prior_sd = 1,
  interval = 0.95,
  seed = 20260811L,
  D = 1
)
```

## Arguments

- items:

  Item-parameter data frame or item collection.

- replications:

  Number of simulation or validation replications.

- posterior_draws:

  Number of posterior draws generated per SBC replication.

- theta_grid:

  Grid of latent-trait values used for numerical scoring or integration.

- prior_mean:

  Mean of the normal latent-trait prior.

- prior_sd:

  Standard deviation of the normal latent-trait prior.

- interval:

  Central posterior interval probability used for coverage assessment.

- seed:

  Random-number seed for reproducible execution.

- D:

  Logistic scaling constant.
