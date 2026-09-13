# Audit a cognitive-diagnosis Q-matrix

Audit a cognitive-diagnosis Q-matrix

## Usage

``` r
eyeprocess_cdm_qmatrix_audit(
  Q,
  item_ids = rownames(Q),
  attribute_names = colnames(Q)
)
```

## Arguments

- Q:

  Binary item-by-attribute Q-matrix.

- item_ids:

  Optional item identifiers.

- attribute_names:

  Optional names for the cognitive-diagnosis attributes.

## Value

An object of class "eye_cdm_qmatrix_audit", stored as a named list, with
components "item", "attribute", "duplicate_rows",
"complete_identity_block". It contains a cognitive-diagnosis Q-matrix
and associated metadata or diagnostics needed to interpret the result.
