# Audit distractor attention patterns

Audit distractor attention patterns

## Usage

``` r
audit_distractor_attention(
  data,
  response_option = "response_option",
  option_gaze,
  chosen_suffix = NULL
)
```

## Arguments

- data:

  Input data frame or compatible tabular object.

- response_option:

  Column identifying the selected response option.

- option_gaze:

  Option-level gaze variables.

- chosen_suffix:

  Suffix identifying the selected option indicator.

## Value

A data frame containing distractor attention patterns. Rows represent
the analysis units and columns contain the identifiers, estimates, or
diagnostics defined by the function.
