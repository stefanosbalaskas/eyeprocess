# Estimate visual exposure probability

Estimate visual exposure probability

## Usage

``` r
estimate_visual_exposure_probability(
  data,
  exposed = "reached",
  predictors,
  family = stats::binomial()
)
```

## Arguments

- data:

  Input data frame or compatible tabular object.

- exposed:

  Value supplied to \`exposed\`; see Details for its model-specific
  role.

- predictors:

  Predictor variables used by the model.

- family:

  Statistical family used by the channel or model.

## Value

An object of class "eye_visual_exposure_model", stored as a named list,
with components "model", "fitted_probability", "exposed", "predictors".
It contains visual exposure probability and associated metadata or
diagnostics needed to interpret the result.
