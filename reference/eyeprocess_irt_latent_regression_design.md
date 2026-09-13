# Build a latent-regression design matrix with explicit centering metadata

Build a latent-regression design matrix with explicit centering metadata

## Usage

``` r
eyeprocess_irt_latent_regression_design(data, formula, center_numeric = TRUE)
```

## Arguments

- data:

  Input data frame, matrix, or compatible analysis object.

- formula:

  Model formula.

- center_numeric:

  Whether numeric predictors are centered.

## Value

An object of class "eye_irt_latent_regression_design", stored as a named
list, with components "matrix", "formula", "centers", "complete". It
contains a latent-regression design matrix with explicit centering
metadata and associated metadata or diagnostics needed to interpret the
result.
