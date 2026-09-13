# Construct a multichannel measurement map

Construct a multichannel measurement map

## Usage

``` r
eyeprocess_multichannel_measurement_map(
  response = "accuracy",
  channels = c("response_time", "dwell", "pupil", "transitions"),
  role = NULL
)
```

## Arguments

- response:

  Observed item response or response variable.

- channels:

  Names or definitions of measurement channels.

- role:

  Declared role of each measurement channel.

## Value

A data frame containing a multichannel measurement map. Rows represent
the analysis units and columns contain the identifiers, estimates, or
diagnostics defined by the function.
