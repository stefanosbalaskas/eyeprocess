# Export minimal RO-Crate 1.3 metadata

Export minimal RO-Crate 1.3 metadata

## Usage

``` r
export_ro_crate_metadata(
  path = "ro-crate-metadata.json",
  name = "eyeprocess analysis",
  description = "Reproducible eyeprocess analysis crate",
  files = NULL,
  creator = NULL,
  license = NULL,
  doi = NULL
)
```

## Arguments

- path:

  Output \`ro-crate-metadata.json\` path.

- name:

  Crate/dataset name.

- description:

  Description.

- files:

  Optional files to include as File entities.

- creator:

  Optional creator name.

- license:

  Optional license URL or identifier.

- doi:

  Optional DOI for the software/data product.
