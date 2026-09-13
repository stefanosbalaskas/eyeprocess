# Audit IRT scale/location identification

Audit IRT scale/location identification

## Usage

``` r
eyeprocess_irt_identification_audit(
  spec,
  constraints = list(),
  n_items = NULL,
  n_persons = NULL
)
```

## Arguments

- spec:

  Model, validation, or analysis specification object.

- constraints:

  Identification or model constraints.

- n_items:

  Number of items.

- n_persons:

  Number of persons.

## Value

An object of class "eye_irt_identification_audit", stored as a named
list, with components "spec", "location_identified", "scale_identified",
"anchors", "warnings", "valid", "n_items", "n_persons". It contains iRT
scale/location identification and associated metadata or diagnostics
needed to interpret the result.
