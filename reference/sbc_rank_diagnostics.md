# Build SBC rank diagnostics

Build SBC rank diagnostics

## Usage

``` r
sbc_rank_diagnostics(ranks, n_draws, bins = NULL)
```

## Arguments

- ranks:

  Integer rank statistics from 0 through \`n_draws\`.

- n_draws:

  Number of posterior draws used per rank.

- bins:

  Histogram bins; defaults to a bounded square-root rule.

## Value

\`eye_sbc_diagnostics\` object.
