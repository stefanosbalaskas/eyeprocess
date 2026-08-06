# Create a deterministic validation job plan

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
validation_job_plan(grid = NULL, replications = 100L, base_seed = 1L,
  model_family = "unspecified", plan_id = NULL, chunk_size = 1L, metadata = list())
```

## Arguments

- grid:

  Scenario grid or named list of factor levels.

- replications:

  Replications per design cell.

- base_seed:

  Base seed used for deterministic seed allocation.

- model_family:

  Model-family label.

- plan_id:

  Optional explicit plan identifier.

- chunk_size:

  Number of jobs assigned to each chunk.

- metadata:

  Arbitrary plan metadata.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
