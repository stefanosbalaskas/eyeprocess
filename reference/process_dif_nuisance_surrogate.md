# Construct a process-data nuisance surrogate for DIF analysis

Construct a process-data nuisance surrogate for DIF analysis

## Usage

``` r
process_dif_nuisance_surrogate(
  data,
  process_features,
  person = "participant_id",
  aggregate = TRUE
)
```

## Arguments

- data:

  Input data frame or compatible tabular object.

- process_features:

  Names of process-derived features.

- person:

  Person or participant identifier column.

- aggregate:

  Aggregation rule for process features.

## Value

A data frame containing a process-data nuisance surrogate for DIF
analysis. Rows represent the analysis units and columns contain the
identifiers, estimates, or diagnostics defined by the function.
