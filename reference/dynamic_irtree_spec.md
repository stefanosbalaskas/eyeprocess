# Specify a dynamic gaze-state IRTree

Specify a dynamic gaze-state IRTree

## Usage

``` r
dynamic_irtree_spec(
  source = c("samples", "visits", "fixations"),
  collapse_consecutive = TRUE,
  include_response = TRUE,
  include_person = FALSE,
  include_item = TRUE
)
```

## Arguments

- source:

  Sequence source.

- collapse_consecutive:

  Collapse consecutive identical states.

- include_response:

  Include item response as a transition predictor.

- include_person:

  Include person fixed effects.

- include_item:

  Include item fixed effects.

## Value

An \`eye_dynamic_irtree_spec\`.
