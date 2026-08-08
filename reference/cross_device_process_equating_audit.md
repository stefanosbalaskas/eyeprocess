# Cross-device process-scale equating audit

Fits a simple affine linking map on anchor observations and reports
residual bias/RMSE by device. It complements, rather than replaces, IRT
anchor linking.

## Usage

``` r
cross_device_process_equating_audit(
  data,
  value,
  reference_value,
  device,
  anchor = NULL
)
```

## Arguments

- data:

  Input data frame or compatible tabular object.

- value:

  Process-value column or values.

- reference_value:

  Value supplied to \`reference_value\`; see Details for its
  model-specific role.

- device:

  Device identifier or device facet.

- anchor:

  Anchor or reference group used for linking.
