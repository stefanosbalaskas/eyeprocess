# Summarize simulation-based calibration ranks

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
validation_sbc_summary(x, by = c("model_family", "scenario_id"), bins = 10L)
```

## Arguments

- x:

  Validation collection or posterior draws data frame.

- by:

  Grouping columns.

- bins:

  Rank-histogram bins.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
