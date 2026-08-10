# Estimate effective sampling frequency from timestamps

Estimate effective sampling frequency from timestamps

## Usage

``` r
effective_sampling_frequency(
  data,
  time = "timestamp_ms",
  unit = c("ms", "s", "us"),
  by = NULL
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
