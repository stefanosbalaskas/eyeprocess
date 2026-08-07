# Spatio-temporal fixation point-process models

Spatio-temporal fixation point-process models. These functions form the
eyeprocess 0.6.0.9000 measurement-intelligence programme and use
dependency-free reference implementations with explicit evidence limits.

## Usage

``` r
fit_fixation_point_process(x, spatial_covariates = NULL, temporal_covariates = NULL,
  interaction = c("none", "self_exciting"), x_col = "x", y_col = "y",
  time_col = "time", grid_size = 20)
fit_marked_gaze_process(x, marks = c("duration", "pupil", "saccade_amplitude"),
  x_col = "x", y_col = "y", time_col = "time")
predict_fixation_intensity(model, new_stimulus = NULL)
diagnose_gaze_point_process(model)
plot_fixation_intensity(x, ...)
plot_spatial_residuals(x, ...)
plot_temporal_excitation_kernel(x, ...)
plot_covariate_effect_surface(x, ...)
plot_observed_expected_fixations(x, ...)
```

## Arguments

- x:

  Input object or data structure appropriate for the selected analysis.

- spatial_covariates:

  Argument controlling \`spatial_covariates\`; see the function usage
  and returned audit metadata.

- temporal_covariates:

  Argument controlling \`temporal_covariates\`; see the function usage
  and returned audit metadata.

- interaction:

  Argument controlling \`interaction\`; see the function usage and
  returned audit metadata.

- x_col:

  Argument controlling \`x_col\`; see the function usage and returned
  audit metadata.

- y_col:

  Argument controlling \`y_col\`; see the function usage and returned
  audit metadata.

- time_col:

  Argument controlling \`time_col\`; see the function usage and returned
  audit metadata.

- grid_size:

  Argument controlling \`grid_size\`; see the function usage and
  returned audit metadata.

- marks:

  Argument controlling \`marks\`; see the function usage and returned
  audit metadata.

- model:

  Argument controlling \`model\`; see the function usage and returned
  audit metadata.

- new_stimulus:

  Argument controlling \`new_stimulus\`; see the function usage and
  returned audit metadata.

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
