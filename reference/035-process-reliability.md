# Process reliability and Generalizability Theory

Process reliability and Generalizability Theory. These functions form
the eyeprocess 0.6.0.9000 measurement-intelligence programme and use
dependency-free reference implementations with explicit evidence limits.

## Usage

``` r
fit_process_gstudy(x, metric, facets = c("person", "item", "session", "device"),
  design = c("crossed", "nested"))
process_variance_components(x)
design_process_dstudy(gstudy, persons = NULL, items = seq(5, 50, 5), sessions = 1:5,
  devices = 1)
audit_process_reliability(x, metrics, method = c("icc", "gtheory", "split_half",
  "bootstrap"), person_col = "person_id", item_col = "item_id", draws = 250)
plot_variance_components(x, ...)
plot_dependability_surface(x, ...)
plot_reliability_by_metric(x, ...)
plot_session_stability(x, ...)
plot_item_sampling_reliability(x, ...)
```

## Arguments

- x:

  Input object or data structure appropriate for the selected analysis.

- metric:

  Argument controlling \`metric\`; see the function usage and returned
  audit metadata.

- facets:

  Argument controlling \`facets\`; see the function usage and returned
  audit metadata.

- design:

  Argument controlling \`design\`; see the function usage and returned
  audit metadata.

- gstudy:

  Argument controlling \`gstudy\`; see the function usage and returned
  audit metadata.

- persons:

  Argument controlling \`persons\`; see the function usage and returned
  audit metadata.

- items:

  Argument controlling \`items\`; see the function usage and returned
  audit metadata.

- sessions:

  Argument controlling \`sessions\`; see the function usage and returned
  audit metadata.

- devices:

  Argument controlling \`devices\`; see the function usage and returned
  audit metadata.

- metrics:

  Argument controlling \`metrics\`; see the function usage and returned
  audit metadata.

- method:

  Argument controlling \`method\`; see the function usage and returned
  audit metadata.

- person_col:

  Argument controlling \`person_col\`; see the function usage and
  returned audit metadata.

- item_col:

  Argument controlling \`item_col\`; see the function usage and returned
  audit metadata.

- draws:

  Argument controlling \`draws\`; see the function usage and returned
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
