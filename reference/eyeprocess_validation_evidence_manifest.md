# Create an evidence manifest from files and in-memory objects

Create an evidence manifest from files and in-memory objects

## Usage

``` r
eyeprocess_validation_evidence_manifest(
  files = character(),
  objects = list(),
  source_commit = NA_character_,
  label = "eyeprocess-0.9-m2"
)
```

## Arguments

- files:

  File paths to include in the evidence manifest.

- objects:

  Objects to include in the evidence manifest.

- source_commit:

  Source-control commit associated with the evidence.

- label:

  Human-readable label.
