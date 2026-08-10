# Run a governed eyeprocess pipeline

Run a governed eyeprocess pipeline

## Usage

``` r
run_eye_pipeline(x, context = list(), stop_on_error = TRUE, previous = NULL)
```

## Arguments

- x:

  Pipeline.

- context:

  Initial named context available as \`.context\`.

- stop_on_error:

  Stop on a non-optional step error.

- previous:

  Optional prior \`eye_pipeline_run\` used for resumption.
