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

## Value

An object of class "eye_validation_evidence_manifest", stored as a named
list, with components "label", "source_commit", "files", "objects",
"generated_at". It contains an evidence manifest from files and
in-memory objects and associated metadata or diagnostics needed to
interpret the result.
