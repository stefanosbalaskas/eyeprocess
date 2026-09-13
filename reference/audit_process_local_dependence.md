# Audit inter-option/process local dependence

Computes pairwise residual correlations (a Q3-style diagnostic) across
item or option columns and, when supplied, analogous process-residual
correlations. This is a diagnostic for local dependence, not a formal
test with universal cutoffs.

## Usage

``` r
audit_process_local_dependence(
  response_residuals,
  process_residuals = NULL,
  threshold = 0.2
)
```

## Arguments

- response_residuals:

  Person-by-item/option residual matrix.

- process_residuals:

  Optional aligned process-residual matrix.

- threshold:

  Absolute correlation threshold used only for flagging.

## Value

An object of class "eye_process_local_dependence_audit", stored as a
named list, with components "pairs", "threshold",
"max_absolute_response", "note". It contains inter-option/process local
dependence and associated metadata or diagnostics needed to interpret
the result.
