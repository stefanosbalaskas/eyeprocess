# Posterior predictive discrepancy table

Posterior predictive discrepancy table

## Usage

``` r
posterior_predictive_discrepancies(
  observed,
  replicated,
  discrepancies = list(mean = function(x) mean(x, na.rm = TRUE), sd = function(x)
    stats::sd(x, na.rm = TRUE), zero_rate = function(x) mean(x == 0, na.rm = TRUE))
)
```

## Arguments

- observed:

  Observed vector/data object.

- replicated:

  List of replicated datasets, or matrix with one replicate per row.

- discrepancies:

  Named list of functions mapping a dataset to one number.
