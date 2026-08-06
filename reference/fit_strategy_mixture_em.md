# Fit the deterministic multi-start EM baseline

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
fit_strategy_mixture_em(prepared, starts = prepared$spec$multiple_starts,
  max_iter = 300L, tolerance = 1e-7, seed = 1L)
```

## Arguments

- prepared:

  Prepared strategy data.

- starts:

  Number of starts.

- max_iter:

  Maximum EM iterations.

- tolerance:

  Relative log-likelihood tolerance.

- seed:

  Seed.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
