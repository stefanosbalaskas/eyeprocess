# Create a machine-readable research decision manifest

Create a machine-readable research decision manifest

## Usage

``` r
eye_decision_manifest(
  sampling = list(),
  validity = list(),
  fixation = list(),
  pupil = list(),
  aoi = list(),
  model = list(),
  sensitivity = list(),
  exclusions = list(),
  provenance = list(),
  notes = NULL,
  ...
)
```

## Arguments

- sampling:

  Sampling decisions.

- validity:

  Validity/missingness decisions.

- fixation:

  Fixation construction decisions.

- pupil:

  Pupil preprocessing decisions.

- aoi:

  AOI assignment decisions.

- model:

  Statistical/psychometric model decisions.

- sensitivity:

  Sensitivity-analysis decisions.

- exclusions:

  Exclusion rules.

- provenance:

  Optional provenance fields.

- notes:

  Notes.

- ...:

  Additional named decision domains.

## Value

An object of class "eye_decision_manifest", stored as a named list, with
components "domains", "notes", "created_at", "schema_version", "status",
"caveat". It contains a machine-readable research decision manifest and
associated metadata or diagnostics needed to interpret the result.
