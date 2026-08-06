# Register vendor-field semantics

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
register_vendor_semantics(corpus_path, vendor, native_field, native_meaning,
  canonical_table, canonical_field, unit = NA_character_, transformation = "identity",
  loss_risk = c("none", "low", "moderate", "high", "unsupported"),
  evidence_case_id = NA_character_)
```

## Arguments

- corpus_path:

  Corpus directory.

- vendor:

  Vendor.

- native_field:

  Native field and meaning.

- native_meaning:

  Native field and meaning.

- canonical_table:

  Canonical destination.

- canonical_field:

  Canonical destination.

- unit:

  Unit.

- transformation:

  Transformation description.

- loss_risk:

  None, low, moderate, high, or unsupported.

- evidence_case_id:

  Supporting case.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
