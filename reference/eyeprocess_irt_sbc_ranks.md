# Construct SBC ranks from scalar truths and posterior draws

Construct SBC ranks from scalar truths and posterior draws

## Usage

``` r
eyeprocess_irt_sbc_ranks(truth, draws, randomize_ties = TRUE, seed = 1L)
```

## Arguments

- truth:

  Known simulated parameter value or vector of true values.

- draws:

  Posterior draws, with draws arranged by simulation case as required.

- randomize_ties:

  Whether ties in SBC ranks are randomized.

- seed:

  Random-number seed for reproducible execution.
