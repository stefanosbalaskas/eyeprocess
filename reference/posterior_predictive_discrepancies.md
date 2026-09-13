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

## Value

An object of class "eye_irt_ppc", "data.frame", stored as a data frame,
containing posterior predictive discrepancy table and associated
metadata needed to interpret the result.
