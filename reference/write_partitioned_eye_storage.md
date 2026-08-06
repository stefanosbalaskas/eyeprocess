# Write an eye dataset as atomic partitioned storage

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
write_partitioned_eye_storage(x, path,
  spec = partition_eye_storage(format = if (requireNamespace("arrow",
  quietly = TRUE)) "parquet" else "rds"), overwrite = FALSE, tables = NULL)
```

## Arguments

- x:

  Eye dataset or named list of data frames.

- path:

  Destination directory.

- spec:

  Partition specification.

- overwrite:

  Whether to replace an existing store.

- tables:

  Tables to write.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
