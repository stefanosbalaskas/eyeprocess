# Fit a multimodal change-point IRT workflow

Fit a multimodal change-point IRT workflow

## Usage

``` r
fit_changepoint_multimodal_irt(
  data,
  ...,
  gaze = "fixation_count",
  refit = TRUE
)
```

## Arguments

- data:

  Input data frame or compatible tabular object.

- ...:

  Additional arguments passed to the selected model, engine, or method.

- gaze:

  Gaze/process variable or column name.

- refit:

  Whether the model is refitted after segmentation.

## Value

An object of class "eye_changepoint_multimodal_irt", stored as a named
list, with components "changepoints", "refit_requested", "status". It
contains a multimodal change-point IRT workflow and associated metadata
or diagnostics needed to interpret the result.
