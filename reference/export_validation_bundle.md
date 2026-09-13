# Export a validation evidence bundle

Export a validation evidence bundle

## Usage

``` r
export_validation_bundle(x, directory, overwrite = FALSE, include_rds = TRUE)
```

## Arguments

- x:

  Validation bundle.

- directory:

  Output directory.

- overwrite:

  Allow writing into a non-empty target directory.

- include_rds:

  Save full R objects as RDS.

## Value

An object of class "eye_validation_export", stored as a named list, with
components "directory", "files", "manifest". It contains a validation
evidence bundle and associated metadata or diagnostics needed to
interpret the result.
