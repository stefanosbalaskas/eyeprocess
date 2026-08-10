# Define a governed pipeline step

Define a governed pipeline step

## Usage

``` r
eye_pipeline_step(
  name,
  fun,
  requires = character(),
  optional = FALSE,
  description = NULL,
  decision = NULL
)
```

## Arguments

- name:

  Unique step name.

- fun:

  Function executed by the step.

- requires:

  Names of upstream steps.

- optional:

  Whether an error may be retained without stopping the pipeline.

- description:

  Human-readable description.

- decision:

  Optional decision label linking the step to an analysis specification.

## Value

An \`eye_pipeline_step\` object.
