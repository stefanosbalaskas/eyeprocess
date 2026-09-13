# Compare outcomes across presentation variants

Compare outcomes across presentation variants

## Usage

``` r
compare_presentation_fairness(data, variant, outcome, person = NULL)
```

## Arguments

- data:

  Data containing presentation version and an outcome.

- variant:

  Presentation-version column.

- outcome:

  Numeric outcome.

- person:

  Optional participant column for descriptive aggregation.

## Value

An object of class "eye_presentation_fairness_comparison", stored as a
named list, with components "model", "summary", "status", "caveat". It
contains outcomes across presentation variants and associated metadata
or diagnostics needed to interpret the result.
