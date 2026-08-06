# Migrate a storage schema through an atomic rewrite

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
migrate_eye_storage_schema(storage, target_path, target_version = "2.0.0",
  format = NULL, overwrite = FALSE)
```

## Arguments

- storage:

  Storage object or path.

- target_path:

  Destination.

- target_version:

  Target schema version.

- format:

  Target format.

- overwrite:

  Whether to replace target.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
