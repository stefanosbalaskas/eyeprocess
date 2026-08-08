# External validation on a completely held-out dataset

External validation on a completely held-out dataset

## Usage

``` r
external_validate_irt(
  train_data,
  external_data,
  fitter,
  predictor,
  scorer,
  label = "external"
)
```

## Arguments

- train_data:

  Value supplied to \`train_data\`; see Details for its model-specific
  role.

- external_data:

  Value supplied to \`external_data\`; see Details for its
  model-specific role.

- fitter:

  Model-fitting function.

- predictor:

  Prediction function.

- scorer:

  Function that scores predictions.

- label:

  Value supplied to \`label\`; see Details for its model-specific role.
