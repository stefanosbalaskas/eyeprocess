# Add or update API lifecycle metadata without global mutation

Add or update API lifecycle metadata without global mutation

## Usage

``` r
register_eye_api_status(
  registry = eye_api_lifecycle(),
  name,
  status,
  canonical = NA_character_,
  replacement = NA_character_,
  since = "0.9.0.9000",
  notes = NA_character_
)
```

## Arguments

- registry:

  Existing lifecycle registry.

- name:

  API name.

- status:

  Lifecycle status.

- canonical:

  Canonical API for the same concept, if applicable.

- replacement:

  Replacement for deprecated/superseded API.

- since:

  Version in which status applies.

- notes:

  Notes.
