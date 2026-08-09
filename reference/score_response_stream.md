# Score a response stream cumulatively

Score a response stream cumulatively

## Usage

``` r
score_response_stream(
  model,
  response_pattern,
  observed_order = NULL,
  method = c("MAP", "EAP"),
  ...
)
```

## Arguments

- model:

  Calibrated \`mirt\` model.

- response_pattern:

  Complete or partial response vector in item order.

- observed_order:

  Order in which observed items arrive. Defaults to sequence.

- method:

  MAP or EAP.

- ...:

  Passed to \`mirt::fscores()\`.

## Value

An \`eye_streaming_score\` object.
