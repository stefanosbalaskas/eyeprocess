# Construct a governed analysis pipeline

Construct a governed analysis pipeline

## Usage

``` r
eye_analysis_pipeline(
  ...,
  spec = eye_analysis_spec(),
  name = "eye_analysis",
  strict = TRUE
)
```

## Arguments

- ...:

  \`eye_pipeline_step\` objects.

- spec:

  Optional \`eye_analysis_spec\`.

- name:

  Pipeline label.

- strict:

  If \`TRUE\`, undeclared dependencies are errors.

## Value

An object of class "eye_analysis_pipeline", stored as a named list, with
components "name", "steps", "spec", "strict", "created_at", "status". It
contains a governed analysis pipeline and associated metadata or
diagnostics needed to interpret the result.
