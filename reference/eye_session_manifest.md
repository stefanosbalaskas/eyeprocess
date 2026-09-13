# Create a session-level provenance manifest

Create a session-level provenance manifest

## Usage

``` r
eye_session_manifest(
  data = NULL,
  files = NULL,
  adapter = NA_character_,
  decisions = NULL,
  pipeline = NULL,
  seeds = NULL,
  notes = NULL
)
```

## Arguments

- data:

  Optional data object.

- files:

  Optional input file paths.

- adapter:

  Adapter/importer identifier.

- decisions:

  Optional decision manifest.

- pipeline:

  Optional pipeline or run object.

- seeds:

  Optional named random seeds.

- notes:

  Optional notes.

## Value

A named list with components "created_utc", "eyeprocess_version",
"data_hash", "files", "adapter", "decisions_hash", "pipeline_hash",
"seeds", "environment", "notes", containing a session-level provenance
manifest and associated metadata or diagnostics.
