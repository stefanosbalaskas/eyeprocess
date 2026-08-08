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
