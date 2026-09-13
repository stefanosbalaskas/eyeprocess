# Audit event survival across an interchange round trip

Audit event survival across an interchange round trip

## Usage

``` r
event_roundtrip_audit(source_events, roundtrip_events, hed_column = NULL, ...)
```

## Arguments

- source_events, roundtrip_events:

  Event tables.

- hed_column:

  Optional HED annotation column to compare structurally.

- ...:

  Arguments forwarded to \`event_semantics_audit()\`.

## Value

An object of class "eye_event_roundtrip_audit", stored as a named list,
with components "status", "event_semantics", "hed". It contains event
survival across an interchange round trip and associated metadata or
diagnostics needed to interpret the result.
