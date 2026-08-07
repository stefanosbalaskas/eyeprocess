# Calibration drift and offline recalibration

Calibration drift and offline recalibration. These functions form the
eyeprocess 0.6.0.9000 measurement-intelligence programme and use
dependency-free reference implementations with explicit evidence limits.

## Usage

``` r
detect_calibration_drift(x, references = NULL, window = "30 sec",
  method = c("targets", "known_aois", "fixation_density"), x_col = NULL,
  y_col = NULL, time_col = NULL)
fit_offline_recalibration(x, method = c("translation", "affine", "polynomial"),
  robust = TRUE, x_col = NULL, y_col = NULL, reference_x_col = NULL,
  reference_y_col = NULL)
apply_offline_recalibration(x, model, x_col = NULL, y_col = NULL,
  suffix = "_recalibrated")
audit_recalibration(before, after, minimum_improvement = NULL)
plot_calibration_vector_field(x, ...)
plot_calibration_error_ellipses(x, ...)
plot_drift_over_time(x, ...)
plot_recalibration_before_after(x, ...)
plot_screen_coverage(x, ...)
```

## Arguments

- x:

  Input object or data structure appropriate for the selected analysis.

- references:

  Argument controlling \`references\`; see the function usage and
  returned audit metadata.

- window:

  Argument controlling \`window\`; see the function usage and returned
  audit metadata.

- method:

  Argument controlling \`method\`; see the function usage and returned
  audit metadata.

- x_col:

  Argument controlling \`x_col\`; see the function usage and returned
  audit metadata.

- y_col:

  Argument controlling \`y_col\`; see the function usage and returned
  audit metadata.

- time_col:

  Argument controlling \`time_col\`; see the function usage and returned
  audit metadata.

- robust:

  Argument controlling \`robust\`; see the function usage and returned
  audit metadata.

- reference_x_col:

  Argument controlling \`reference_x_col\`; see the function usage and
  returned audit metadata.

- reference_y_col:

  Argument controlling \`reference_y_col\`; see the function usage and
  returned audit metadata.

- model:

  Argument controlling \`model\`; see the function usage and returned
  audit metadata.

- suffix:

  Argument controlling \`suffix\`; see the function usage and returned
  audit metadata.

- before:

  Argument controlling \`before\`; see the function usage and returned
  audit metadata.

- after:

  Argument controlling \`after\`; see the function usage and returned
  audit metadata.

- minimum_improvement:

  Argument controlling \`minimum_improvement\`; see the function usage
  and returned audit metadata.

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
