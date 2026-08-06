# Benchmark storage formats and query operations

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
benchmark_eye_storage(x, formats = c("rds", "csv", "parquet"),
  partition_by = c("participant_id", "recording_id"), repetitions = 3L,
  directory = tempdir())
```

## Arguments

- x:

  Named list of tables or eye dataset.

- formats:

  Formats to benchmark.

- partition_by:

  Partition columns.

- repetitions:

  Repetitions.

- directory:

  Parent temporary directory.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
