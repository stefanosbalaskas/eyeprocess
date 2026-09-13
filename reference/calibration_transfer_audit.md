# Audit transfer of calibration across devices/sessions/sites

Audit transfer of calibration across devices/sessions/sites

## Usage

``` r
calibration_transfer_audit(data, group, observed, predicted)
```

## Arguments

- data:

  Data containing group, observed and predicted values.

- group:

  Grouping column.

- observed:

  Observed binary/numeric outcome column.

- predicted:

  Predicted probability/numeric score column.

## Value

An object of class "eye_calibration_transfer_audit", "data.frame",
stored as a data frame, containing transfer of calibration across
devices/sessions/sites and associated metadata needed to interpret the
result.
