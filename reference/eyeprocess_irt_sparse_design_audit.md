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
