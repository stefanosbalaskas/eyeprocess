# Declare a deprecation in a structured form

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
eyeprocess_deprecation(old, replacement, since, remove_after, reason = "")
```

## Arguments

- old:

  Deprecated symbol.

- replacement:

  Replacement symbol.

- since:

  Version where deprecation started.

- remove_after:

  Earliest removal version.

- reason:

  Reason.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
