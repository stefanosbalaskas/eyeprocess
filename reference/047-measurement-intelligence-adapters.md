# Measurement-intelligence compatibility adapters

Measurement-intelligence compatibility adapters. These functions form
the eyeprocess 0.6.0.9000 measurement-intelligence programme and use
dependency-free reference implementations with explicit evidence limits.

## Usage

``` r
fit_process_missingness_model(x, observed, predictors, random = c("person", "item"))
crossmodal_recurrence_model(x, y, outcome = NULL, channels = c("gaze_pupil",
  "gaze_eda", "pupil_eda"), radius = NULL, covariates = NULL)
```

## Arguments

- x:

  Input object or data structure appropriate for the selected analysis.

- observed:

  Argument controlling \`observed\`; see the function usage and returned
  audit metadata.

- predictors:

  Argument controlling \`predictors\`; see the function usage and
  returned audit metadata.

- random:

  Argument controlling \`random\`; see the function usage and returned
  audit metadata.

- y:

  Argument controlling \`y\`; see the function usage and returned audit
  metadata.

- outcome:

  Argument controlling \`outcome\`; see the function usage and returned
  audit metadata.

- channels:

  Argument controlling \`channels\`; see the function usage and returned
  audit metadata.

- radius:

  Argument controlling \`radius\`; see the function usage and returned
  audit metadata.

- covariates:

  Argument controlling \`covariates\`; see the function usage and
  returned audit metadata.

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
