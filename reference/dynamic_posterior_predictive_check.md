# Posterior predictive checks for dynamic state models

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
dynamic_posterior_predictive_check(object, draws = 200L, seed = 1L)
```

## Arguments

- object:

  Dynamic IRTree fit using the Stan engine.

- draws:

  Maximum posterior predictive draws to summarize.

- seed:

  Seed used when subsampling posterior draws.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
