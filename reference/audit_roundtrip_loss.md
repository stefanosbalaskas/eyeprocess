# Audit semantic and numerical loss after a round trip

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
audit_roundtrip_loss(source, roundtrip, tables = canonical_table_names(),
  tolerance = 1e-8)
```

## Arguments

- source:

  Source and re-imported \`eye_dataset\` objects.

- roundtrip:

  Source and re-imported \`eye_dataset\` objects.

- tables:

  Canonical tables to compare.

- tolerance:

  Numeric tolerance.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
