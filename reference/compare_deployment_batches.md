# Compare two deployment batches descriptively

Compare two deployment batches descriptively

## Usage

``` r
compare_deployment_batches(
  data,
  batch = "deployment_batch",
  batch_a,
  batch_b,
  metrics = NULL,
  item = "item_id"
)
```

## Arguments

- data:

  Deployment data.

- batch:

  Batch column.

- batch_a, batch_b:

  Values to compare.

- metrics:

  Metrics to compare.

- item:

  Optional item identifier for item-matched differences.

## Value

A data frame containing two deployment batches descriptively. Rows
represent the analysis units and columns contain the identifiers,
estimates, or diagnostics defined by the function.
