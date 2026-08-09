# Run biometric-feature imputation as a sensitivity analysis

Run biometric-feature imputation as a sensitivity analysis

## Usage

``` r
biometric_imputation_sensitivity(
  data,
  variables,
  methods = c("mice", "missForest"),
  m = 3L,
  maxit = 3L,
  seed = 521
)
```

## Arguments

- data:

  Data containing biometric/process features.

- variables:

  Variables to impute.

- methods:

  Any of \`mice\` and \`missForest\`.

- m:

  Number of MICE imputations.

- maxit:

  Iteration count.

- seed:

  Seed.

## Value

An \`eye_biometric_imputation_sensitivity\` object. Complete-case
analysis is not replaced automatically.
