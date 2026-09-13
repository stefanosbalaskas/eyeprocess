# Summarise response-time structure for joint IRT work

Summarise response-time structure for joint IRT work

## Usage

``` r
eyeprocess_response_time_profile(data, person, item, response_time)
```

## Arguments

- data:

  Input data frame, matrix, or compatible analysis object.

- person:

  Name of the person identifier column.

- item:

  Name of the item identifier column.

- response_time:

  Response-time variable or values.

## Value

An object of class "eye_response_time_profile", stored as a named list,
with components "item", "person", "n". It contains response-time
structure for joint IRT work and associated metadata or diagnostics
needed to interpret the result.
