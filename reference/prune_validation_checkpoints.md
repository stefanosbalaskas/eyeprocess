# Remove obsolete or corrupt validation checkpoints

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
prune_validation_checkpoints(path, statuses = c("corrupt", "locked"), dry_run = TRUE)
```

## Arguments

- path:

  Validation output directory.

- statuses:

  Statuses to remove.

- dry_run:

  Report without deleting.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
