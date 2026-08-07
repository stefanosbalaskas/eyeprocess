# Cross-device and cross-vendor metric linking

Cross-device and cross-vendor metric linking. These functions form the
eyeprocess 0.6.0.9000 measurement-intelligence programme and use
dependency-free reference implementations with explicit evidence limits.

## Usage

``` r
fit_device_linking(x, metric, reference_device, method = c("mixed_bland_altman",
  "hierarchical", "equipercentile"), device_col = "device", id_cols = c("person_id",
  "item_id"))
apply_device_linking(x, linking_model, metric = NULL, device_col = NULL,
  output_col = NULL)
audit_device_equivalence(x, equivalence_margin, by = c("metric", "task", "aoi"))
estimate_device_specific_error(x)
plot_device_agreement(x, ...)
plot_device_bias_by_magnitude(x, ...)
plot_device_transfer_curve(x, ...)
plot_device_equivalence_intervals(x, ...)
plot_cross_vendor_metric_matrix(x, ...)
```

## Arguments

- x:

  Input object or data structure appropriate for the selected analysis.

- metric:

  Argument controlling \`metric\`; see the function usage and returned
  audit metadata.

- reference_device:

  Argument controlling \`reference_device\`; see the function usage and
  returned audit metadata.

- method:

  Argument controlling \`method\`; see the function usage and returned
  audit metadata.

- device_col:

  Argument controlling \`device_col\`; see the function usage and
  returned audit metadata.

- id_cols:

  Argument controlling \`id_cols\`; see the function usage and returned
  audit metadata.

- linking_model:

  Argument controlling \`linking_model\`; see the function usage and
  returned audit metadata.

- output_col:

  Argument controlling \`output_col\`; see the function usage and
  returned audit metadata.

- equivalence_margin:

  Argument controlling \`equivalence_margin\`; see the function usage
  and returned audit metadata.

- by:

  Argument controlling \`by\`; see the function usage and returned audit
  metadata.

- ...:

  Additional arguments passed to the underlying method or plotting
  function.

## Details

The APIs return auditable S3 objects. Plot wrappers call registered
base-graphics methods. Experimental or approximate engines are labelled
in object status fields and should be validated before confirmatory or
operational use.

## Value

An eyeprocess result object, data frame, model object, plot, or audit
table as documented by the individual function.

## See also

[`plot_diagnostics()`](https://stefanosbalaskas.github.io/eyeprocess/reference/030-measurement-intelligence-utils.md),
[`plot_evidence()`](https://stefanosbalaskas.github.io/eyeprocess/reference/030-measurement-intelligence-utils.md),
and
[`plot_sensitivity()`](https://stefanosbalaskas.github.io/eyeprocess/reference/030-measurement-intelligence-utils.md).
