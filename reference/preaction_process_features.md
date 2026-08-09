# Build pre-action process features

Build pre-action process features

## Usage

``` r
preaction_process_features(
  data,
  by = c("person_id", "trial_id"),
  time = "time_ms",
  response_time = "response_time_ms",
  windows_ms = c(500, 1000, 2000),
  aoi = "aoi",
  pupil = "pupil_bc",
  blink = "blink"
)
```

## Arguments

- data:

  Sample-level data.

- by:

  Grouping columns.

- time, response_time:

  Time and response-event columns.

- windows_ms:

  Positive look-back windows before action.

- aoi, pupil, blink:

  Optional feature columns.
