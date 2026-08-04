# Write an eye dataset to RDS or Arrow/Parquet storage

Write an eye dataset to RDS or Arrow/Parquet storage

## Usage

``` r
write_eye_storage(
  x,
  path,
  format = c("rds", "parquet", "arrow_dataset"),
  tables = canonical_table_names(),
  partitioning = NULL,
  compression = "zstd",
  overwrite = FALSE,
  retain_metadata = TRUE
)
```

## Arguments

- x:

  An \`eye_dataset\`.

- path:

  Output path.

- format:

  Storage format.

- tables:

  Canonical tables to write.

- partitioning:

  Optional partition columns for Arrow datasets.

- compression:

  Parquet compression codec.

- overwrite:

  Whether to replace an existing target.

- retain_metadata:

  Whether to retain raw/vendor metadata in a sidecar RDS.

## Value

An \`eye_storage\` handle.
