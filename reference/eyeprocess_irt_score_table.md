# Score a response matrix with EAP, MAP, or ML

Score a response matrix with EAP, MAP, or ML

## Usage

``` r
eyeprocess_irt_score_table(
  responses,
  items,
  method = c("EAP", "MAP", "ML"),
  person_ids = rownames(responses),
  ...
)
```

## Arguments

- responses:

  Response matrix or response data.

- items:

  Item-parameter data frame or item collection.

- method:

  Scoring, linking, or analysis method.

- person_ids:

  Optional person identifiers.

- ...:

  Additional arguments passed to the selected method or external engine.
