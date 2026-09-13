# Audit multidimensional IRT loading coverage

Audit multidimensional IRT loading coverage

## Usage

``` r
eyeprocess_mirt_loading_audit(spec, min_items_per_dimension = 3L)
```

## Arguments

- spec:

  Model, validation, or analysis specification object.

- min_items_per_dimension:

  Minimum number of items required per dimension.

## Value

A data frame containing multidimensional IRT loading coverage. Rows
represent the analysis units and columns contain the identifiers,
estimates, or diagnostics defined by the function.
