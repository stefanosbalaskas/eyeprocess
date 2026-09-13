# Summarise missingness patterns across response and process channels

Summarise missingness patterns across response and process channels

## Usage

``` r
eyeprocess_process_missingness_pattern(data, response, channels)
```

## Arguments

- data:

  Input data frame, matrix, or compatible analysis object.

- response:

  Observed item response or response variable.

- channels:

  Names or definitions of measurement channels.

## Value

A data frame containing missingness patterns across response and process
channels. Rows represent the analysis units and columns contain the
identifiers, estimates, or diagnostics defined by the function.
