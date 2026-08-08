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
