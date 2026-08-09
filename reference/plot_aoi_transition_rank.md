# Plot top AOI transitions by probability/count

Plot top AOI transitions by probability/count

## Usage

``` r
plot_aoi_transition_rank(
  data,
  from = "from",
  to = "to",
  normalize = c("from", "all", "none"),
  top_n = 20L,
  ...
)
```

## Arguments

- data:

  Data frame containing the required process variables.

- from:

  Name of the column identifying the transition origin.

- to:

  Name of the column identifying the transition destination.

- normalize:

  Normalization rule applied to transition counts or weights.

- top_n:

  Maximum number of highest-ranked entries to display.

- ...:

  Additional arguments passed to the underlying method or helper.
