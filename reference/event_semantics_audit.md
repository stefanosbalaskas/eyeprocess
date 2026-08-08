# Audit event semantic preservation

Audit event semantic preservation

## Usage

``` r
event_semantics_audit(
  source_events,
  roundtrip_events,
  label = "event",
  time = "timestamp",
  key = NULL,
  tolerance = 1e-06
)
```

## Arguments

- source_events, roundtrip_events:

  Event tables.

- label:

  Event-label field.

- time:

  Event-time field; set \`NULL\` to compare labels only.

- key:

  Optional event identity key.

- tolerance:

  Timestamp tolerance.
