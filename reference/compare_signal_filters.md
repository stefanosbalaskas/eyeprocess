# Compare multiple signal filters

Compare multiple signal filters

## Usage

``` r
compare_signal_filters(
  signal,
  widths = c(5L, 9L, 15L),
  methods = c("runmed", "robfilter")
)
```

## Arguments

- signal:

  Numeric signal.

- widths:

  Widths to compare.

- methods:

  Methods to compare.

## Value

A tabular R object containing multiple signal filters; rows represent
analysis units and columns contain the returned quantities.
