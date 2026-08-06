# Write a validation release report

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
write_validation_release_report(x, path, completion = NULL, promotion = NULL,
  title = "eyeprocess validation release report", include_session = TRUE)
```

## Arguments

- x:

  Validation collection.

- path:

  Markdown output file.

- completion:

  Optional completion audit.

- promotion:

  Optional model-promotion audit.

- title:

  Report title.

- include_session:

  Include session information.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
