# Validate strategy posteriors against an experimental manipulation

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
validate_strategy_manipulation(object, condition, expected_strategy,
  minimum_contrast = 0)
```

## Arguments

- object:

  Fitted strategy-mixture object.

- condition:

  Condition column in the original trial data.

- expected_strategy:

  Named character vector mapping condition values to prespecified
  strategy labels.

- minimum_contrast:

  Minimum mean posterior-probability contrast over alternative
  strategies.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
