# Interval coverage calibration curve

Interval coverage calibration curve

## Usage

``` r
coverage_calibration_curve(truth, lower, upper, nominal = NULL)
```

## Arguments

- truth:

  True values.

- lower:

  Matrix/data.frame of lower limits or numeric vector.

- upper:

  Matrix/data.frame of upper limits or numeric vector.

- nominal:

  Nominal coverage labels, one per interval column.

## Value

A data frame containing interval coverage calibration curve. Rows
represent the analysis units and columns contain the identifiers,
estimates, or diagnostics defined by the function.
