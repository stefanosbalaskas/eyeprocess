# Validate an M2 fit or simulation

For simulations, performs data/support and identifiability checks. For
fitted models, additionally audits MCMC diagnostics and optionally
channel-specific posterior predictive checks.

## Usage

``` r
validate_multimodal_m2(x, include_ppc = TRUE, rhat_max = 1.01, ess_min = 200)
```

## Arguments

- x:

  M2 simulation or fit.

- include_ppc:

  Include posterior predictive checks for fitted M2 models.

- rhat_max:

  Maximum acceptable R-hat.

- ess_min:

  Minimum bulk/tail ESS threshold.

## Value

An \`eye_multimodal_m2_validation\`.
