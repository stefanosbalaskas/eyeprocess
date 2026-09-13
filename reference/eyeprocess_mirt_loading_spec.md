# Declare a multidimensional IRT loading structure

Declare a multidimensional IRT loading structure

## Usage

``` r
eyeprocess_mirt_loading_spec(
  items,
  loadings,
  dimension_names = colnames(loadings),
  simple_structure = FALSE
)
```

## Arguments

- items:

  Item-parameter data frame or item collection.

- loadings:

  Item-by-dimension loading matrix.

- dimension_names:

  Optional names for latent dimensions.

- simple_structure:

  Whether a simple-structure loading pattern is required.

## Value

An object of class "eye_mirt_loading_spec", stored as a named list, with
components "items", "loadings", "dimensions", "simple_structure",
"violations". It contains declare a multidimensional IRT loading
structure and associated metadata or diagnostics needed to interpret the
result.
