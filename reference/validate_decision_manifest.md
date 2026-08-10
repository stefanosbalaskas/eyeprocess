# Validate a research decision manifest

Validate a research decision manifest

## Usage

``` r
validate_decision_manifest(
  x,
  required_domains = c("sampling", "validity", "fixation", "pupil", "aoi", "model",
    "sensitivity", "exclusions"),
  require_nonempty = FALSE
)
```

## Arguments

- x:

  Manifest.

- required_domains:

  Domains that must exist.

- require_nonempty:

  If TRUE, required domains must contain at least one decision.
