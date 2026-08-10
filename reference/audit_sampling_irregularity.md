# Audit sampling irregularity

Audit sampling irregularity

## Usage

``` r
audit_sampling_irregularity(
  data,
  time = "timestamp_ms",
  unit = c("ms", "s", "us"),
  by = NULL,
  cv_threshold = 0.05
)
```

## Arguments

- data:

  Sample data.

- time:

  Timestamp column.

- unit:

  Timestamp unit.

- by:

  Optional grouping columns.

- cv_threshold:

  Review threshold for interval coefficient of variation.
