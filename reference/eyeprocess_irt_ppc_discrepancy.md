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

## Value

An object of class "eye_irt_ppc_discrepancy", stored as a named list,
with components "statistic", "observed", "replicated",
"posterior_predictive_p", "interval". It contains observed and
replicated IRT discrepancy statistics and associated metadata or
diagnostics needed to interpret the result.
