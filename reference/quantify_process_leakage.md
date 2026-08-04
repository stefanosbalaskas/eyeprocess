# Quantify leakage from row-wise rather than grouped validation

Quantify leakage from row-wise rather than grouped validation

## Usage

``` r
quantify_process_leakage(
  data,
  formula,
  group = c("participant_id", "item_id"),
  v = 5L,
  seed = 1L
)
```

## Arguments

- data:

  Data frame.

- formula:

  Binary-outcome formula.

- group:

  Grouping columns.

- v:

  Number of folds.

- seed:

  Random seed.

## Value

Comparison table.
