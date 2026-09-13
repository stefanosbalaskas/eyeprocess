# Build a paper-ready IRT information/precision table

Build a paper-ready IRT information/precision table

## Usage

``` r
eyeprocess_irt_precision_evidence_table(
  items,
  theta = seq(-3, 3, by = 0.5),
  digits = 4L
)
```

## Arguments

- items:

  Item-parameter data frame or item collection.

- theta:

  Latent-trait value or vector of latent-trait values.

- digits:

  Number of decimal digits used for presentation.

## Value

An R object containing a paper-ready IRT information/precision table.
The concrete class and structure follow the selected method, engine, or
input object and are preserved as documented by that workflow.
