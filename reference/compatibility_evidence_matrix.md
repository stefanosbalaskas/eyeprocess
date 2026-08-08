# Build a detailed compatibility evidence matrix

Build a detailed compatibility evidence matrix

## Usage

``` r
compatibility_evidence_matrix(compatibility, evidence = NULL)
```

## Arguments

- compatibility:

  Existing output from \`build_compatibility_matrix()\` or a compatible
  data frame.

- evidence:

  Case-level evidence with columns \`ecosystem\`, \`device\`,
  \`evidence_level\`, and optionally \`semantic_roundtrip_pass\`.

## Value

An \`eye_compatibility_evidence_matrix\` object.
