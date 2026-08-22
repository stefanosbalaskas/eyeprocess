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

  Parquet compression codec. For writes, the default \`"zstd"\` is
  preferred; when the argument is omitted and that codec is unavailable,
  storage falls back to \`"snappy"\` and then \`"uncompressed"\`. An
  explicitly requested unavailable codec errors.

- overwrite:

  Whether to replace an existing target.

- retain_metadata:

  Whether to retain raw/vendor metadata in a sidecar RDS.

## Value

An \`eye_storage\` handle.
