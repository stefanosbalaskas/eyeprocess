# Grade model evidence against an explicit validation contract

This is intentionally conservative: failure of any required criterion
caps the evidence grade. It does not convert exploratory evidence into a
claim of substantive validity.

## Usage

``` r
grade_model_evidence(
  recovery,
  spec = irt_validation_spec("unspecified"),
  external_validation = NULL,
  sbc = NULL,
  ppc = NULL,
  semantic_roundtrip = NULL
)
```

## Arguments

- recovery:

  Value supplied to \`recovery\`; see Details for its model-specific
  role.

- spec:

  IRT model or validation specification.

- external_validation:

  Value supplied to \`external_validation\`; see Details for its
  model-specific role.

- sbc:

  Value supplied to \`sbc\`; see Details for its model-specific role.

- ppc:

  Value supplied to \`ppc\`; see Details for its model-specific role.

- semantic_roundtrip:

  Value supplied to \`semantic_roundtrip\`; see Details for its
  model-specific role.
