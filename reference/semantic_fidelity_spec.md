# Semantic fidelity specification

Semantic fidelity specification

## Usage

``` r
semantic_fidelity_spec(
  timestamp_tolerance = 1e-06,
  coordinate_tolerance = 1e-06,
  pupil_tolerance = 1e-06,
  missingness_tolerance = 1e-06,
  correlation_floor = 0.999,
  allow_row_reorder = TRUE
)
```

## Arguments

- timestamp_tolerance:

  Absolute tolerance after time-unit normalization.

- coordinate_tolerance:

  Absolute tolerance after coordinate normalization.

- pupil_tolerance:

  Absolute tolerance after pupil-unit normalization.

- missingness_tolerance:

  Maximum tolerated absolute change in missingness.

- correlation_floor:

  Correlation floor used when deciding whether a numeric transformation
  remains semantically equivalent.

- allow_row_reorder:

  Whether row reordering is permitted when a key is supplied.

## Value

An object of class \`eye_semantic_fidelity_spec\`.
