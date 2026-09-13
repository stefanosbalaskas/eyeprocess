# Summarise item exposure rates

Summarise item exposure rates

## Usage

``` r
eyeprocess_irt_exposure_summary(
  administered,
  item_bank_ids = unique(administered)
)
```

## Arguments

- administered:

  Identifiers or records for administered items.

- item_bank_ids:

  Complete set of item identifiers in the bank.

## Value

A data frame containing item exposure rates. Rows represent the analysis
units and columns contain the identifiers, estimates, or diagnostics
defined by the function.
