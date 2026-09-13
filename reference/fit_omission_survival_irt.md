# Response/RT/omission survival IRT reference model

Fits a response model among reached/answered observations and
cause-specific survival models for omission and not-reached processes.
The function keeps the missingness mechanisms distinct by construction.

## Usage

``` r
fit_omission_survival_irt(
  data,
  response = "response",
  response_time = "response_time",
  omission_time = NULL,
  reached = "reached",
  person = "participant_id",
  item = "item_id",
  gaze_exposure = NULL,
  first_fixation_latency = NULL,
  ...
)
```

## Arguments

- data:

  Input data frame or compatible tabular object.

- response:

  Response variable or response-column name.

- response_time:

  Response-time variable or column name.

- omission_time:

  Time associated with an omitted response.

- reached:

  Indicator that the item was reached.

- person:

  Person or participant identifier column.

- item:

  Item identifier, name, or item column.

- gaze_exposure:

  Gaze-based exposure measure.

- first_fixation_latency:

  Latency to first fixation.

- ...:

  Additional arguments passed to the selected model, engine, or method.

## Value

An object of class "eye_omission_survival_irt", stored as a named list,
with components "response_model", "omission_model", "not_reached_model",
"classified_data", "state_counts", "status", "note". It contains
response/RT/omission survival IRT reference model and associated
metadata or diagnostics needed to interpret the result.
