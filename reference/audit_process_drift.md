# Audit post-deployment psychometric and biometric drift

Audit post-deployment psychometric and biometric drift

## Usage

``` r
audit_process_drift(
  data,
  item = "item_id",
  batch = "deployment_batch",
  metrics = c("irt_difficulty", "irt_discrimination", "rt_ms", "dwell_ms", "pupil_bc",
    "valid_gaze_prop", "screen_luminance"),
  spec = process_drift_spec(),
  reference_batch = NULL,
  aggregate_fun = .ep08_mean
)
```

## Arguments

- data:

  Item-by-batch or trial-level deployment data.

- item:

  Item identifier column.

- batch:

  Ordered deployment batch/date column.

- metrics:

  Numeric metrics to monitor.

- spec:

  Drift specification.

- reference_batch:

  Optional reference batch value(s).

- aggregate_fun:

  Aggregation function used when multiple rows occur within item x
  batch.
