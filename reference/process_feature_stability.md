# Summarize process-feature stability across repeated analyses

Summarize process-feature stability across repeated analyses

## Usage

``` r
process_feature_stability(
  data,
  feature = "feature",
  split = "split",
  importance = "importance",
  top_n = 20L
)
```

## Arguments

- data:

  Long table containing feature names and ranks/importance values.

- feature:

  Feature column.

- split:

  Split/resample column.

- importance:

  Importance column, where larger is better.

- top_n:

  Number of top features counted per split.
