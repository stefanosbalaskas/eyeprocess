# Audit SBC rank uniformity

Uses binned chi-square diagnostics as a coarse screening diagnostic and
also reports the mean/variance of normalized ranks. It is not a
replacement for rank-histogram inspection.

## Usage

``` r
audit_sbc(x, bins = 10L, alpha = 0.01)
```

## Arguments

- x:

  Object to print, plot, summarize, or audit.

- bins:

  Number of bins used by the diagnostic.

- alpha:

  Significance or tail-probability level.
