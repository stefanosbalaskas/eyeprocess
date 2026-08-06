# Evaluate dynamic-state recovery under misclassification

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
dynamic_irtree_recovery(grid = expand.grid(state_misclassification = c(0, 0.05, 0.15),
  missing_state = c(0, 0.10), stringsAsFactors = FALSE), replications = 20L,
  spec = dynamic_irtree_spec(engine = "multinomial"), base_seed = 1L)
```

## Arguments

- grid:

  Scenario grid.

- replications:

  Replications per scenario.

- spec:

  Dynamic IRTree specification.

- base_seed:

  Base seed.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
