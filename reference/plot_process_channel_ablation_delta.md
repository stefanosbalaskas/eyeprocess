# Plot channel-ablation delta from a full/reference model

Plot channel-ablation delta from a full/reference model

## Usage

``` r
plot_process_channel_ablation_delta(
  x = NULL,
  table = NULL,
  channel_col = "channel",
  metric_col = "metric",
  value_col = "value",
  full_label = "full",
  metric = NULL,
  ...
)
```

## Arguments

- x:

  \`eye_process_channel_ablation\` object or compatible table.

- table:

  Optional explicit table.

- channel_col, metric_col, value_col:

  Column names.

- full_label:

  Full/reference channel label.

- metric:

  Metric to evaluate or display.

- ...:

  Additional arguments passed to the underlying method or helper.
