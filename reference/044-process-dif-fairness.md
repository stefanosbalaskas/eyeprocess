# Dynamic process-DIF and fairness drift

Dynamic process-DIF and fairness drift. These functions form the
eyeprocess 0.6.0.9000 measurement-intelligence programme and use
dependency-free reference implementations with explicit evidence limits.

## Usage

``` r
fit_process_dif(x, response, process, group, item, ability = NULL)
monitor_dif_drift(x, time, group, metrics, item = NULL)
decompose_dif_evidence(psychometric, process = NULL, design_features = NULL)
audit_fairness_transportability(x, context = "device", effect = "process_dif",
  item = "item_id")
plot_group_icc_process_overlay(x, ...)
plot_process_dif_forest(x, ...)
plot_dif_drift_heatmap(x, ...)
plot_fairness_transport_matrix(x, ...)
plot_item_group_process_curves(x, ...)
```

## Arguments

- x:

  Input object or data structure appropriate for the selected analysis.

- response:

  Argument controlling \`response\`; see the function usage and returned
  audit metadata.

- process:

  Argument controlling \`process\`; see the function usage and returned
  audit metadata.

- group:

  Argument controlling \`group\`; see the function usage and returned
  audit metadata.

- item:

  Argument controlling \`item\`; see the function usage and returned
  audit metadata.

- ability:

  Argument controlling \`ability\`; see the function usage and returned
  audit metadata.

- time:

  Argument controlling \`time\`; see the function usage and returned
  audit metadata.

- metrics:

  Argument controlling \`metrics\`; see the function usage and returned
  audit metadata.

- psychometric:

  Argument controlling \`psychometric\`; see the function usage and
  returned audit metadata.

- design_features:

  Argument controlling \`design_features\`; see the function usage and
  returned audit metadata.

- context:

  Argument controlling \`context\`; see the function usage and returned
  audit metadata.

- effect:

  Argument controlling \`effect\`; see the function usage and returned
  audit metadata.

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
