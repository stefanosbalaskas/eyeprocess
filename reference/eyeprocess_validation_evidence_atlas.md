# Assemble a validation evidence atlas

The atlas is an organizational object. It preserves links between
claims, software-validation results, figures, tables, and provenance; it
does not upgrade software-validation evidence into construct-validity
evidence.

## Usage

``` r
eyeprocess_validation_evidence_atlas(
  claims,
  recovery = NULL,
  sbc = NULL,
  stress = NULL,
  reliability = NULL,
  negative_controls = NULL,
  irt = NULL,
  provenance = NULL,
  artifacts = NULL
)
```

## Arguments

- claims:

  Claim-evidence mapping table.

- recovery:

  Parameter-recovery evidence object or table.

- sbc:

  Simulation-based-calibration evidence object or table.

- stress:

  Measurement-stress evidence object or table.

- reliability:

  Reliability or repeatability evidence object or table.

- negative_controls:

  Negative-control evidence object or table.

- irt:

  IRT-specific evidence object or table.

- provenance:

  Provenance metadata or provenance object.

- artifacts:

  Artifact table or file-index information.
