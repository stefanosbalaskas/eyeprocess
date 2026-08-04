# Construct the advanced-model validation grid

By default this returns a one-factor-at-a-time screening design around a
declared reference scenario. This preserves every factor and level from
the research programme without accidentally launching hundreds of
thousands of Monte Carlo scenarios. Set \`full_factorial = TRUE\` only
when the computing plan explicitly supports the complete Cartesian
design.

## Usage

``` r
advanced_validation_grid(quick = FALSE, full_factorial = FALSE)
```

## Arguments

- quick:

  Whether to return a compact smoke-test design.

- full_factorial:

  Whether to return the complete Cartesian design.

## Value

A scenario data frame for \`run_model_validation()\` or custom Monte
Carlo programmes.
