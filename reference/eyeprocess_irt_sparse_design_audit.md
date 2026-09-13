# Audit sparse person-item response coverage

Audit sparse person-item response coverage

## Usage

``` r
eyeprocess_irt_sparse_design_audit(
  data,
  person,
  item,
  response = NULL,
  min_person_items = 3L,
  min_item_persons = 10L
)
```

## Arguments

- data:

  Input data frame, matrix, or compatible analysis object.

- person:

  Name of the person identifier column.

- item:

  Name of the item identifier column.

- response:

  Observed item response or response variable.

- min_person_items:

  Minimum number of observed items required per person.

- min_item_persons:

  Minimum number of observed persons required per item.

## Value

An object of class "eye_irt_sparse_design_audit", stored as a named
list, with components "n_persons", "n_items", "n_observed", "density",
"person_counts", "item_counts", "sparse_persons", "sparse_items",
"min_person_items", "min_item_persons". It contains sparse person-item
response coverage and associated metadata or diagnostics needed to
interpret the result.
