# Audit a complete semantic round trip

Audit a complete semantic round trip

## Usage

``` r
semantic_roundtrip_audit(
  source,
  roundtrip,
  key = NULL,
  fields = NULL,
  timestamp = list(),
  coordinates = list(),
  pupil = NULL,
  eye = NULL,
  source_events = NULL,
  roundtrip_events = NULL,
  event_args = list()
)
```

## Arguments

- source:

  Original canonical samples.

- roundtrip:

  Canonical samples reconstructed after interchange.

- key:

  Optional row identity key.

- fields:

  Fields for field-level comparison.

- timestamp:

  Optional list of arguments forwarded to
  \`timestamp_fidelity_audit()\`.

- coordinates:

  Optional list of arguments forwarded to
  \`coordinate_fidelity_audit()\`.

- pupil:

  Optional list of arguments forwarded to
  \`pupil_unit_fidelity_audit()\`.

- eye:

  Optional list of arguments forwarded to
  \`eye_stream_fidelity_audit()\`.

- source_events, roundtrip_events:

  Optional event tables.

- event_args:

  Optional event-audit arguments.

## Value

An \`eye_semantic_roundtrip\` object.
