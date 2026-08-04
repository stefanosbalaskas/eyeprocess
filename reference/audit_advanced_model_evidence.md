# Audit advanced-model scientific evidence

Audit advanced-model scientific evidence

## Usage

``` r
audit_advanced_model_evidence(evidence, spec = advanced_model_evidence_spec())
```

## Arguments

- evidence:

  Named list keyed by model function. Each model may contain
  \`recovery\`, \`calibration\`, \`misspecification\`,
  \`grouped_validation\`, \`engine_equivalence\`,
  \`empirical_reproduction\`, and \`sensitivity\` objects.

- spec:

  Evidence specification.

## Value

An \`eye_advanced_evidence_audit\` data frame.
