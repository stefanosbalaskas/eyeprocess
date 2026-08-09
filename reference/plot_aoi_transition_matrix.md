# Plot an AOI transition matrix

Plot an AOI transition matrix

## Usage

``` r
plot_aoi_transition_matrix(
  data,
  from = "from",
  to = "to",
  normalize = c("from", "all", "none"),
  ...
)
```

## Arguments

- data:

  Transition-pair data.

- from, to:

  Column names.

- normalize:

  Normalize within from-AOI, globally, or not at all.

- ...:

  Additional arguments passed to the underlying method or helper.
