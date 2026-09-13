# Classify item missingness using exposure and response evidence

Classify item missingness using exposure and response evidence

## Usage

``` r
classify_item_missingness(
  data,
  response = "response",
  reached = "reached",
  inspected = NULL,
  started = NULL
)
```

## Arguments

- data:

  Input data frame or compatible tabular object.

- response:

  Response variable or response-column name.

- reached:

  Indicator that the item was reached.

- inspected:

  Indicator that the item or response area was inspected.

- started:

  Indicator that responding was initiated.

## Value

An object of class "factor", stored as an R object, containing classify
item missingness using exposure and response evidence and associated
metadata needed to interpret the result.
