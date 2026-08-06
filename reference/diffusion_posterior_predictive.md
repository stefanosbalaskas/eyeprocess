# Posterior predictive summaries for accuracy and RT

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
diffusion_posterior_predictive(object, draws = 200L, method = c("rtdists",
  "stan_proxy"), seed = 1L)
```

## Arguments

- object:

  Diffusion fit.

- draws:

  Number of generated-quantity draws to retain.

- method:

  Value for \`method\`. See the function description and relevant
  article for constraints.

- seed:

  Value for \`seed\`. See the function description and relevant article
  for constraints.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
