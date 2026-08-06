# Resume incomplete or failed validation jobs

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
resume_validation_jobs(plan, output_dir, retry = c("missing", "failed",
  "nonconverged", "locked", "corrupt"), ...)
```

## Arguments

- plan:

  Validation plan or manifest path.

- output_dir:

  Validation output directory.

- retry:

  Which statuses to re-run.

- ...:

  Passed to \`run_validation_jobs()\`.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
