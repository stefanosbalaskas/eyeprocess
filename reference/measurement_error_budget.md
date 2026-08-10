# Build a non-collapsed measurement-error budget

Build a non-collapsed measurement-error budget

## Usage

``` r
measurement_error_budget(
  accuracy = NA_real_,
  precision = NA_real_,
  data_loss = NA_real_,
  effective_hz = NA_real_,
  calibration_drift = NA_real_,
  units = NULL
)
```

## Arguments

- accuracy:

  Measurement inaccuracy/offset metric.

- precision:

  Measurement imprecision metric.

- data_loss:

  Data-loss proportion.

- effective_hz:

  Effective sampling frequency.

- calibration_drift:

  Optional drift metric.

- units:

  Optional named units.
