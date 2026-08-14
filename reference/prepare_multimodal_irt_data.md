# Prepare a canonical multimodal person-item-trial measurement object

Creates a loss-aware person-item-trial table for response, response
time, gaze, and pupil channels while retaining references to optional
sample-level or sequence payloads. No time series are silently
aggregated.

## Usage

``` r
prepare_multimodal_irt_data(
  data,
  person,
  item,
  trial = NULL,
  response = NULL,
  rt = NULL,
  gaze = NULL,
  pupil = NULL,
  quality = character(),
  device = NULL,
  payloads = list(),
  provenance = list()
)
```

## Arguments

- data:

  A data.frame containing one row per intended person-item-trial.

- person, item, trial:

  Column names identifying the measurement keys.

- response, rt, gaze, pupil:

  Optional column names for channel summaries.

- quality:

  Optional character vector of quality-field names.

- device:

  Optional list or data.frame of device metadata.

- payloads:

  Named list of sample-level/sequence payloads.

- provenance:

  Optional provenance list.

## Value

An \`eye_multimodal_measurement\` object.
