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
