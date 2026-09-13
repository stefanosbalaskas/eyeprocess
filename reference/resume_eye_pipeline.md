# Resume a governed pipeline from a prior run

Resume a governed pipeline from a prior run

## Usage

``` r
resume_eye_pipeline(x, previous, context = list(), stop_on_error = TRUE)
```

## Arguments

- x:

  Pipeline.

- previous:

  Prior pipeline run.

- context:

  Context used for new steps.

- stop_on_error:

  Stop on non-optional error.

## Value

An object of class "eye_pipeline_run", stored as a named list, with
components "pipeline", "pipeline_hash", "outputs", "records", "errors",
"warnings", "context_hash", "completed", "created_at", "status". It
contains resume a governed pipeline from a prior run and associated
metadata or diagnostics needed to interpret the result.
