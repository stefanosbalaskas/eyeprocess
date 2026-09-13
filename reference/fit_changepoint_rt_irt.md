# Fit a change-point RT IRT workflow

Fit a change-point RT IRT workflow

## Usage

``` r
fit_changepoint_rt_irt(data, ..., refit = TRUE)
```

## Arguments

- data:

  Input data frame or compatible tabular object.

- ...:

  Additional arguments passed to the selected model, engine, or method.

- refit:

  Whether the model is refitted after segmentation.

## Value

An object of class "eye_changepoint_rt_irt", stored as a named list,
with components "changepoints", "refit_requested", "status". It contains
a change-point RT IRT workflow and associated metadata or diagnostics
needed to interpret the result.
