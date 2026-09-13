# Fit a single AOI growth curve

Fit a single AOI growth curve

## Usage

``` r
fit_aoi_growth_curve(data, time, outcome, degree = 3L)
```

## Arguments

- data:

  Data frame.

- time:

  Time column.

- outcome:

  Numeric AOI proportion/indicator column.

- degree:

  Polynomial degree.

## Value

An object of class "eye_aoi_growth_curve", stored as a named list, with
components "model", "time", "outcome", "degree", "poly", "range",
"status". It contains a single AOI growth curve and associated metadata
or diagnostics needed to interpret the result.
