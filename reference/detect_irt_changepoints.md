# Detect IRT/process change points using an SIC-inspired multichannel score

This implementation is a transparent package reference inspired by the
2026 SIC-CPA literature. It combines Bernoulli response likelihood with
normal log-RT and standardized gaze likelihoods. It is not a
line-for-line reproduction of the article's estimator.

## Usage

``` r
detect_irt_changepoints(
  data,
  person = "participant_id",
  order = "item_order",
  response = "response",
  rt = "rt",
  gaze = NULL,
  min_segment = 5L,
  min_delta_sic = 2,
  max_changes = 2L
)
```

## Arguments

- data:

  Input data frame or compatible tabular object.

- person:

  Person or participant identifier column.

- order:

  Within-sequence ordering variable.

- response:

  Response variable or response-column name.

- rt:

  Response-time variable or column name.

- gaze:

  Gaze/process variable or column name.

- min_segment:

  Minimum segment length.

- min_delta_sic:

  Minimum information-criterion improvement.

- max_changes:

  Maximum number of change points.
