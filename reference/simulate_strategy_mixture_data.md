# Simulate a theory-defined strategy-mixture study

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
simulate_strategy_mixture_data(n_person = 100L, n_item = 20L, signatures,
  trials_per_item = 1L, strategy_prevalence = NULL, feature_sd = 0.6, seed = 1L)
```

## Arguments

- n_person:

  Number of participants.

- n_item:

  Number of items.

- signatures:

  Strategy signature matrix.

- trials_per_item:

  Trials per person-item combination.

- strategy_prevalence:

  Strategy prevalence.

- feature_sd:

  Feature residual SD.

- seed:

  Seed.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
