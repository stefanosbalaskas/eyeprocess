# Evaluate a table against named validation rules

Evaluate a table against named validation rules

## Usage

``` r
validation_acceptance_matrix(summary, rules, id_cols = character())
```

## Arguments

- summary:

  Validation summary table.

- rules:

  Collection of validation acceptance rules.

- id_cols:

  Columns identifying validation scenarios.

## Value

A tabular R object containing a table against named validation rules;
rows represent analysis units and columns contain the returned
quantities.
