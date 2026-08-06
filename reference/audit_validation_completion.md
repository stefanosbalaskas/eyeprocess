# Audit whether a validation programme is complete

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
audit_validation_completion(x, thresholds = validation_thresholds(),
  empirical_reproduction = NULL)
```

## Arguments

- x:

  Validation collection.

- thresholds:

  Validation thresholds.

- empirical_reproduction:

  Optional empirical-reproduction evidence required when configured in
  \`thresholds\`.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
