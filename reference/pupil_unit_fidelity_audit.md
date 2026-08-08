# Pupil-unit semantic-fidelity audit

Pupil-unit semantic-fidelity audit

## Usage

``` r
pupil_unit_fidelity_audit(
  source,
  roundtrip,
  source_pupil = "pupil_size",
  roundtrip_pupil = source_pupil,
  key = NULL,
  tolerance = 1e-06,
  correlation_floor = 0.995
)
```

## Arguments

- source:

  Original/source representation.

- roundtrip:

  Round-tripped or comparison representation.

- source_pupil:

  Source pupil-measure column.

- roundtrip_pupil:

  Round-tripped pupil-measure column.

- key:

  Column or columns used to align records.

- tolerance:

  Numerical tolerance used by the comparison.

- correlation_floor:

  Minimum correlation treated as compatible.
