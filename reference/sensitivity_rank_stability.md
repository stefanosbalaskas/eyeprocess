# Rank stability across specifications

Rank stability across specifications

## Usage

``` r
sensitivity_rank_stability(x, id = NULL, rank = NULL, specification = NULL)
```

## Arguments

- x:

  Data frame or list of ranking vectors.

- id:

  Optional item identifier when x is a long data frame.

- rank:

  Optional rank/value column when x is a long data frame.

- specification:

  Optional specification column.

## Value

An R object containing rank stability across specifications. The
concrete class and structure follow the selected method, engine, or
input object and are preserved as documented by that workflow.
