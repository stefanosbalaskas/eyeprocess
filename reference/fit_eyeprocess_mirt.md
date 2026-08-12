# Fit a model with mirt without substituting another estimator

Fit a model with mirt without substituting another estimator

## Usage

``` r
fit_eyeprocess_mirt(data, model = 1, itemtype = "2PL", ..., engine = "mirt")
```

## Arguments

- data:

  Input data frame, matrix, or compatible analysis object.

- model:

  Model specification passed to the selected external engine.

- itemtype:

  Item type specification for mirt.

- ...:

  Additional arguments passed to the selected method or external engine.

- engine:

  Requested estimation or analysis engine.
