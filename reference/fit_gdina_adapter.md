# fit gdina adapter

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
fit_gdina_adapter(data, Q, model = "GDINA", purpose = "cognitive diagnosis", ...)
```

## Arguments

- data:

  Value for \`data\`. See the function description and relevant article
  for constraints.

- Q:

  Q-matrix with one row per item.

- model:

  GDINA model specification used for eye-dataset inputs.

- purpose:

  Declared scientific purpose for the external-engine contract.

- ...:

  Additional arguments passed to the selected engine or method.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
