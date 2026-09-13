# Build a provenance edge table

Build a provenance edge table

## Usage

``` r
provenance_edge_table(from, to, relation = "wasDerivedFrom")
```

## Arguments

- from, to:

  Node identifiers.

- relation:

  PROV-like relation labels.

## Value

A data frame containing a provenance edge table. Rows represent the
analysis units and columns contain the identifiers, estimates, or
diagnostics defined by the function.
