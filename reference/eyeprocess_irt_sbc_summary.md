# Summarise IRT SBC ranks with the package SBC diagnostics

Summarise IRT SBC ranks with the package SBC diagnostics

## Usage

``` r
eyeprocess_irt_sbc_summary(ranks, n_draws, bins = NULL)
```

## Arguments

- ranks:

  Simulation-based-calibration rank values.

- n_draws:

  Number of posterior draws underlying each rank.

- bins:

  Number of bins used for rank-distribution summaries.

## Value

An object of class "eye_irt_sbc_evidence", stored as a named list, with
components "diagnostics", "ecdf_deviation", "n", "n_draws". It contains
iRT SBC ranks with the package SBC diagnostics and associated metadata
or diagnostics needed to interpret the result.
