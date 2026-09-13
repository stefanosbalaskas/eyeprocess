# Audit missing-by-design structure in an IRT response matrix

Audit missing-by-design structure in an IRT response matrix

## Usage

``` r
eyeprocess_irt_missing_by_design_audit(
  responses,
  design = NULL,
  min_administered = 1L
)
```

## Arguments

- responses:

  Response matrix or response data.

- design:

  Validation or simulation design object.

- min_administered:

  Minimum number of administered items required for a record.

## Value

An object of class "eye_irt_missing_design_audit", stored as a named
list, with components "n_persons", "n_items", "observed_fraction",
"administered_per_person", "administered_per_item", "sparse_persons",
"structural_missing", "unexpected_missing", "has_declared_design". It
contains missing-by-design structure in an IRT response matrix and
associated metadata or diagnostics needed to interpret the result.
