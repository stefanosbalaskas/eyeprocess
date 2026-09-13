# Audit preservation of monocular/binocular stream semantics

Audit preservation of monocular/binocular stream semantics

## Usage

``` r
eye_stream_fidelity_audit(
  source,
  roundtrip,
  source_eye = "eye",
  roundtrip_eye = source_eye,
  key = NULL
)
```

## Arguments

- source:

  Original/source representation.

- roundtrip:

  Round-tripped or comparison representation.

- source_eye:

  Source recorded-eye column.

- roundtrip_eye:

  Round-tripped recorded-eye column.

- key:

  Column or columns used to align records.

## Value

An object of class "eye_stream_fidelity", stored as a named list, with
components "status", "matched_n", "source_streams", "roundtrip_streams",
"confusion". It contains preservation of monocular/binocular stream
semantics and associated metadata or diagnostics needed to interpret the
result.
