# Score a partial response pattern from a calibrated mirt model

Score a partial response pattern from a calibrated mirt model

## Usage

``` r
score_partial_response_pattern(
  model,
  response_pattern,
  method = c("MAP", "EAP"),
  ...
)
```

## Arguments

- model:

  Calibrated \`mirt\` model.

- response_pattern:

  Full-length response vector with future/unobserved items as NA.

- method:

  mirt scoring method, typically MAP or EAP.

- ...:

  Passed to \`mirt::fscores()\`.
