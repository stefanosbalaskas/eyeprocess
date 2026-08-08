# Coordinate semantic-fidelity audit

Detects lossless preservation and stable affine coordinate
transformations.

## Usage

``` r
coordinate_fidelity_audit(
  source,
  roundtrip,
  source_x = "x",
  source_y = "y",
  roundtrip_x = source_x,
  roundtrip_y = source_y,
  key = NULL,
  tolerance = 1e-06,
  correlation_floor = 0.999
)
```

## Arguments

- source:

  Original/source representation.

- roundtrip:

  Round-tripped or comparison representation.

- source_x:

  Source horizontal coordinate column.

- source_y:

  Source vertical coordinate column.

- roundtrip_x:

  Round-tripped horizontal coordinate column.

- roundtrip_y:

  Round-tripped vertical coordinate column.

- key:

  Column or columns used to align records.

- tolerance:

  Numerical tolerance used by the comparison.

- correlation_floor:

  Minimum correlation treated as compatible.
