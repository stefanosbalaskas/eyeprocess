# Fit a user-defined OpenMx process model

Fit a user-defined OpenMx process model

## Usage

``` r
fit_openmx_process_model(x, model_builder, include_features = TRUE, ...)
```

## Arguments

- x:

  An \`eye_dataset\`.

- model_builder:

  Function receiving model data and returning an OpenMx model.

- include_features:

  Whether to merge process features.

- ...:

  Passed to \`OpenMx::mxRun()\`.

## Value

An \`eyeprocess_model\`.
