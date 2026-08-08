# Field-level semantic fidelity report

Classifies canonical fields after a semantic round trip. Character
fields are compared exactly after NA preservation; numeric fields are
compared directly and also tested for a stable affine transform.

## Usage

``` r
field_fidelity_report(
  source,
  roundtrip,
  fields = NULL,
  mapping = NULL,
  key = NULL,
  tolerance = 1e-08,
  spec = semantic_fidelity_spec()
)
```

## Arguments

- source:

  Original canonical data.

- roundtrip:

  Canonical data reconstructed after an interchange round trip.

- fields:

  Fields to compare. Defaults to common fields.

- mapping:

  Optional named character vector mapping source field names to
  round-trip field names.

- key:

  Optional unique row-alignment fields.

- tolerance:

  Numeric equality tolerance.

- spec:

  A \`semantic_fidelity_spec()\` object.

## Value

An object of class \`eye_field_fidelity_report\`.
