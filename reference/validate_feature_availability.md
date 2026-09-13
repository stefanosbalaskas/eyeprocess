# Validate feature availability against an analysis cutoff

Validate feature availability against an analysis cutoff

## Usage

``` r
validate_feature_availability(provenance, cutoff)
```

## Arguments

- provenance:

  Feature provenance table.

- cutoff:

  Scalar cutoff or named vector by feature.

## Value

A data frame containing feature availability against an analysis cutoff.
Rows represent the analysis units and columns contain the identifiers,
estimates, or diagnostics defined by the function.
