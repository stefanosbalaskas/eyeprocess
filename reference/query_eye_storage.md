# Query partitioned eye storage lazily where possible

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
query_eye_storage(storage, table, filters = list(), columns = NULL, collect = TRUE)
```

## Arguments

- storage:

  Storage object or path.

- table:

  Canonical table.

- filters:

  Named list of equality filters.

- columns:

  Optional selected columns.

- collect:

  Whether to collect an Arrow query.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
