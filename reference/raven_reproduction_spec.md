# Specify a published Raven strategy-model reproduction

Specify a published Raven strategy-model reproduction

## Usage

``` r
raven_reproduction_spec(
  data_path,
  response,
  strategy_features,
  published_targets = NULL,
  licence_reviewed = FALSE,
  citation = "10.1016/j.intell.2023.101782"
)
```

## Arguments

- data_path:

  Path to the exact public data/materials.

- response:

  Response field.

- strategy_features:

  Theory-defined eye-tracking strategy indicators.

- published_targets:

  Optional named target estimates.

- licence_reviewed:

  Whether data and code reuse has been reviewed.

- citation:

  Citation or DOI for the reproduced analysis.

## Value

An \`eye_raven_reproduction_spec\`.
