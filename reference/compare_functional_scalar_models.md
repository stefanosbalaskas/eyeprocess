# Compare functional and scalar pupil summaries

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
compare_functional_scalar_models(x, scalar_features = c("pupil_peak", "pupil_auc",
  "pupil_mean"), criterion = c("AIC", "log_loss"), folds = 5L, seed = 1L)
```

## Arguments

- x:

  Functional pupil fit or prepared data.

- scalar_features:

  Scalar feature names.

- criterion:

  AIC or cross-validated log loss.

- folds:

  Grouped folds for cross-validation.

- seed:

  Random seed.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
