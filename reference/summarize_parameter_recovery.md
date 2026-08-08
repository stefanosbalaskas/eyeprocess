# Summarise parameter recovery

Summarise parameter recovery

## Usage

``` r
summarize_parameter_recovery(
  results,
  by = c("scenario", "engine", "parameter"),
  interval_level = 0.95
)
```

## Arguments

- results:

  Canonical or raw recovery results.

- by:

  Grouping columns.

- interval_level:

  Nominal interval level, used only for labelling.
