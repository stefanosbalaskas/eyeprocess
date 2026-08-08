# Iteratively detect, clean, and recalibrate after process change points

Iteratively detect, clean, and recalibrate after process change points

## Usage

``` r
recalibrate_after_changepoint(
  data,
  fitter,
  person = "participant_id",
  order = "item_order",
  policy = c("flag", "exclude_post_change", "add_regime"),
  ...
)
```

## Arguments

- data:

  Input data frame or compatible tabular object.

- fitter:

  Function accepting a data frame and returning a calibration fit.

- person:

  Person or participant identifier column.

- order:

  Within-sequence ordering variable.

- policy:

  \`flag\`, \`exclude_post_change\`, or \`add_regime\`.

- ...:

  Additional arguments passed to the selected model, engine, or method.
