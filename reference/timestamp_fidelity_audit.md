# Timestamp semantic-fidelity audit

Timestamp semantic-fidelity audit

## Usage

``` r
timestamp_fidelity_audit(
  source,
  roundtrip,
  source_time = "timestamp",
  roundtrip_time = source_time,
  source_unit = "seconds",
  roundtrip_unit = source_unit,
  key = NULL,
  tolerance = 1e-06
)
```

## Arguments

- source:

  Original data.

- roundtrip:

  Round-tripped data.

- source_time:

  Source timestamp column.

- roundtrip_time:

  Round-trip timestamp column.

- source_unit, roundtrip_unit:

  One of seconds, milliseconds, microseconds, or nanoseconds.

- key:

  Optional alignment key.

- tolerance:

  Seconds-scale tolerance after normalization.

## Value

An \`eye_timestamp_fidelity\` object.
