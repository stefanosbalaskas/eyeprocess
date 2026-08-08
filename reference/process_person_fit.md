# Joint response-process person-fit diagnostic

Produces model-discrepancy evidence; it never labels a participant as
dishonest, impaired, disengaged, or otherwise psychologically
categorized.

## Usage

``` r
process_person_fit(
  object,
  data = NULL,
  person = NULL,
  response_weight = 1,
  rt_weight = 1,
  process_weight = 1
)
```

## Arguments

- object:

  A fitted eyeprocess model or audit object.

- data:

  Input data frame or compatible tabular object.

- person:

  Person or participant identifier column.

- response_weight:

  Weight assigned to response discrepancy.

- rt_weight:

  Weight assigned to response-time discrepancy.

- process_weight:

  Weight assigned to process discrepancy.
