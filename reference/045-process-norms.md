# Conditional process reference centiles

Conditional process reference centiles. These functions form the
eyeprocess 0.6.0.9000 measurement-intelligence programme and use
dependency-free reference implementations with explicit evidence limits.

## Usage

``` r
fit_process_norms(x, metric, covariates, family = c("auto", "gaussian",
  "lognormal"))
predict_process_centiles(model, newdata, centiles = c(2.5, 10, 25, 50, 75, 90,
  97.5))
score_process_deviation(model, newdata, type = c("z", "centile",
  "tail_probability"))
audit_norm_transportability(model, new_sample)
plot_process_centiles(x, ...)
plot_normative_fan(x, ...)
plot_person_normative_profile(x, ...)
plot_item_normative_deviation(x, ...)
```

## Arguments

- x:

  Input object or data structure appropriate for the selected analysis.

- metric:

  Argument controlling \`metric\`; see the function usage and returned
  audit metadata.

- covariates:

  Argument controlling \`covariates\`; see the function usage and
  returned audit metadata.

- family:

  Argument controlling \`family\`; see the function usage and returned
  audit metadata.

- model:

  Argument controlling \`model\`; see the function usage and returned
  audit metadata.

- newdata:

  Argument controlling \`newdata\`; see the function usage and returned
  audit metadata.

- centiles:

  Argument controlling \`centiles\`; see the function usage and returned
  audit metadata.

- type:

  Argument controlling \`type\`; see the function usage and returned
  audit metadata.

- new_sample:

  Argument controlling \`new_sample\`; see the function usage and
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
