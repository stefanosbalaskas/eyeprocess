# Plot process-feature stability across resamples/splits

Plot process-feature stability across resamples/splits

## Usage

``` r
plot_process_feature_stability(
  data,
  feature = "feature",
  stability = "selection_rate",
  top_n = 20L,
  ...
)
```

## Arguments

- data:

  Table with feature and stability/rank information.

- feature:

  Feature column.

- stability:

  Stability/selection-rate column.

- top_n:

  Maximum features shown.

- ...:

  Additional arguments passed to the underlying method or helper.
