# Simulate observed dynamic-state transitions

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
simulate_dynamic_irtree_data(n_person = 100L, n_item = 20L,
  transitions_per_trial = 8L, states = c("prompt", "evidence", "options"),
  beta_response = 0.5, person_sd = 0.4, item_sd = 0.3, irregular_time = TRUE,
  state_misclassification = 0, missing_state = 0, structural_zeros = NULL, seed = 1L)
```

## Arguments

- n_person:

  Number of persons and items.

- n_item:

  Number of persons and items.

- transitions_per_trial:

  Number of transitions per person-item trial.

- states:

  State labels.

- beta_response:

  Response effect on transitions.

- person_sd:

  Person/item heterogeneity.

- item_sd:

  Person/item heterogeneity.

- irregular_time:

  Whether to generate irregular time gaps.

- state_misclassification:

  Destination-state error probability.

- missing_state:

  Missing destination-state probability.

- structural_zeros:

  Optional forbidden transitions.

- seed:

  Random seed.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
