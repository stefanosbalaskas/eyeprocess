# Promote a case support level only when evidence is supplied

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
promote_vendor_support(corpus_path, case_id, level = c("fixture-tested",
  "empirically-validated"), validation, reviewer, notes = NA_character_)
```

## Arguments

- corpus_path:

  Corpus directory.

- case_id:

  Case identifier.

- level:

  New support level.

- validation:

  Validation result/audit.

- reviewer:

  Reviewer identifier.

- notes:

  Notes.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
