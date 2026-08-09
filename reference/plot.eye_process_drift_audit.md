# Plot process drift audit diagnostics

Plot process drift audit diagnostics

## Usage

``` r
# S3 method for class 'eye_process_drift_audit'
plot(
  x,
  type = c("trajectory", "delta", "heatmap", "control"),
  metric = NULL,
  item = NULL,
  ...
)
```

## Arguments

- x:

  Object to process, inspect, compare, or plot.

- type:

  Type of summary or visual representation to produce.

- metric:

  Metric to evaluate or display.

- item:

  Optional item identifier used to restrict or highlight results.

- ...:

  Additional arguments passed to the underlying method or helper.
