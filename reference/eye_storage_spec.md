# Specify disk-backed eyeprocess storage

Specify disk-backed eyeprocess storage

## Usage

``` r
eye_storage_spec(
  path,
  format = c("rds", "parquet", "arrow_dataset"),
  tables = canonical_table_names(),
  partitioning = NULL,
  compression = "zstd"
)
```

## Arguments

- path:

  Storage directory or RDS file.

- format:

  One of \`"rds"\`, \`"parquet"\`, or \`"arrow_dataset"\`.

- tables:

  Canonical tables to store.

- partitioning:

  Optional Arrow partition columns.

- compression:

  Parquet compression codec. For writes, the default \`"zstd"\` is
  preferred; when the argument is omitted and that codec is unavailable,
  storage falls back to \`"snappy"\` and then \`"uncompressed"\`. An
  explicitly requested unavailable codec errors.

## Value

An \`eye_storage_spec\` object.
