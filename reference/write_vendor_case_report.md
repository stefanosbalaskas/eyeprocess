# Write a vendor case evidence report

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
write_vendor_case_report(corpus_path, case_id, path, validation = NULL,
  roundtrip = NULL)
```

## Arguments

- corpus_path:

  Corpus directory.

- case_id:

  Case identifier.

- path:

  Markdown output path.

- validation:

  Optional validation result.

- roundtrip:

  Optional round-trip audit.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
