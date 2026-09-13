# GPIRT model-criticism interface

\`external\` is the only exact GPIRT path. \`spline_reference\` fits
flexible logistic spline IRFs solely as a nonparametric stress test for
conventional logistic IRF shape; it is deliberately not described as a
Gaussian process.

## Usage

``` r
fit_gpirt(
  response_matrix,
  engine = c("spline_reference", "external"),
  external_engine = NULL,
  spline_df = 5L,
  ...
)
```

## Arguments

- response_matrix:

  Person-by-item response matrix.

- engine:

  Estimation engine.

- external_engine:

  Validated external fitting function.

- spline_df:

  Degrees of freedom for the spline reference model.

- ...:

  Additional arguments passed to the selected model, engine, or method.

## Value

An object of class "eye_gpirt", stored as a named list, with components
"response_matrix", "models", "theta_proxy", "engine", "exact_gpirt",
"status", "note". It contains gPIRT model-criticism interface and
associated metadata or diagnostics needed to interpret the result.
