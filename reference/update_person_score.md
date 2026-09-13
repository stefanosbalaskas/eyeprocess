# Update a partial person score with one new response

Update a partial person score with one new response

## Usage

``` r
update_person_score(
  model,
  current_pattern,
  item_position,
  response,
  method = c("MAP", "EAP"),
  ...
)
```

## Arguments

- model:

  Calibrated model.

- current_pattern:

  Existing full-length partial pattern.

- item_position:

  Item receiving the new response.

- response:

  New response.

- method:

  Scoring method.

- ...:

  Additional arguments passed to the underlying method or helper.

## Value

A named list with components "pattern", "score", containing update a
partial person score with one new response and associated metadata or
diagnostics.
