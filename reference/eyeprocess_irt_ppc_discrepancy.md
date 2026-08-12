# Compare observed and replicated IRT discrepancy statistics

Compare observed and replicated IRT discrepancy statistics

## Usage

``` r
eyeprocess_irt_ppc_discrepancy(
  observed,
  replicated,
  statistic = c("mean_score", "score_sd", "item_means", "max_item_residual")
)
```

## Arguments

- observed:

  Observed responses or observed values.

- replicated:

  Replicated data or replicated statistic values.

- statistic:

  Discrepancy statistic or statistic function.
