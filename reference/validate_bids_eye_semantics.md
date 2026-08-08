# Validate BIDS eye-tracking semantics

Implements a lightweight contract for BIDS 1.11.1 eye-tracking
physiology data. It does not replace the BIDS Validator.

## Usage

``` r
validate_bids_eye_semantics(data, metadata, events_metadata = NULL)
```

## Arguments

- data:

  Eye-tracking physiology table.

- metadata:

  Parsed JSON sidecar as a named list.

- events_metadata:

  Optional event-sidecar metadata containing \`StimulusPresentation\`
  information for gaze-on-screen recordings.

## Value

An \`eye_bids_semantic_audit\` object.
