# Collect validation checkpoints from one or more directories

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
collect_validation_jobs(path, plan = NULL, strict = TRUE)
```

## Arguments

- path:

  Manifest/output directory or character vector of directories.

- plan:

  Optional validation plan.

- strict:

  Fail when duplicate job identifiers disagree.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
