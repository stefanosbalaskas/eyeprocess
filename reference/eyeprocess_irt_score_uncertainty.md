# Summarise score uncertainty

Summarise score uncertainty

## Usage

``` r
eyeprocess_irt_score_uncertainty(scores)
```

## Arguments

- scores:

  Score object or score table.

## Value

An object of class "eye_irt_score_uncertainty", stored as a named list,
with components "n", "mean_se", "median_se", "p95_se",
"marginal_reliability". It contains score uncertainty and associated
metadata or diagnostics needed to interpret the result.
