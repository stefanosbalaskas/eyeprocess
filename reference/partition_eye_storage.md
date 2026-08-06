# Create a partition specification

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
partition_eye_storage(by = c("participant_id", "session_id", "recording_id"),
  format = c("parquet", "csv", "rds"), compression = "zstd", max_rows = 1000000L)
```

## Arguments

- by:

  Partition columns.

- format:

  Storage format.

- compression:

  Compression codec for Parquet.

- max_rows:

  Maximum rows per physical file.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
