# Describe speed-accuracy association without causal interpretation

Describe speed-accuracy association without causal interpretation

## Usage

``` r
eyeprocess_speed_accuracy_profile(data, person, response, response_time)
```

## Arguments

- data:

  Input data frame, matrix, or compatible analysis object.

- person:

  Name of the person identifier column.

- response:

  Observed item response or response variable.

- response_time:

  Response-time variable or values.

## Value

An object of class "eye_speed_accuracy_profile", stored as a named list,
with components "person", "pooled_correlation", "guardrail". It contains
describe speed-accuracy association without causal interpretation and
associated metadata or diagnostics needed to interpret the result.
