# Informative missingness and MNAR sensitivity

Informative missingness and MNAR sensitivity. These functions form the
eyeprocess 0.6.0.9000 measurement-intelligence programme and use
dependency-free reference implementations with explicit evidence limits.

## Usage

``` r
fit_process_observation_model(x, observed, predictors, random = c("person", "item"))
fit_joint_signal_missingness(outcome, observation, method = c("selection",
  "shared_parameter"), x = NULL, predictors = NULL)
process_pattern_mixture(x, delta = seq(-1, 1, 0.1), metric = NULL, estimand = mean)
sensitivity_mnar_process(x, estimand = mean, null = 0, ...)
plot_observation_probability(x, ...)
plot_missingness_by_time(x, ...)
plot_missingness_by_aoi(x, ...)
plot_mnar_tipping_point(x, ...)
plot_complete_case_sensitivity(x, ...)
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

- outcome:

  Argument controlling \`outcome\`; see the function usage and returned
  audit metadata.

- observation:

  Argument controlling \`observation\`; see the function usage and
  returned audit metadata.

- method:

  Argument controlling \`method\`; see the function usage and returned
  audit metadata.

- delta:

  Argument controlling \`delta\`; see the function usage and returned
  audit metadata.

- metric:

  Argument controlling \`metric\`; see the function usage and returned
  audit metadata.

- estimand:

  Argument controlling \`estimand\`; see the function usage and returned
  audit metadata.

- null:

  Argument controlling \`null\`; see the function usage and returned
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
