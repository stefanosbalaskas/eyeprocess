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
