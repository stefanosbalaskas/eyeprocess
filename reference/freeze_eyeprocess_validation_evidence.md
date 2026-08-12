# Freeze a complete Milestone \#2 evidence bundle

Freeze a complete Milestone \#2 evidence bundle

## Usage

``` r
freeze_eyeprocess_validation_evidence(
  design,
  recovery = NULL,
  sbc = NULL,
  stress = NULL,
  reliability = NULL,
  negative_controls = NULL,
  irt = NULL,
  claims = NULL,
  provenance = NULL,
  source_commit = NA_character_
)
```

## Arguments

- design:

  Validation or simulation design object.

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

- claims:

  Claim-evidence mapping table.

- provenance:

  Provenance metadata or provenance object.

- source_commit:

  Source-control commit associated with the evidence.
