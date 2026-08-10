# Audit decision provenance and completeness

Audit decision provenance and completeness

## Usage

``` r
audit_decision_provenance(
  x,
  required_domains = c("sampling", "validity", "fixation", "pupil", "aoi", "model",
    "sensitivity", "exclusions"),
  required_provenance = c("data_source", "software_version", "analysis_commit")
)
```

## Arguments

- x:

  Manifest.

- required_domains:

  Required decision domains.

- required_provenance:

  Provenance keys expected under \`provenance\`.
