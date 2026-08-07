# Process measurement-uncertainty budgets

Process measurement-uncertainty budgets. These functions form the
eyeprocess 0.6.0.9000 measurement-intelligence programme and use
dependency-free reference implementations with explicit evidence limits.

## Usage

``` r
process_uncertainty_spec(calibration = TRUE, aoi_assignment = TRUE,
  preprocessing = TRUE, sampling = TRUE, model = TRUE, source_sd = NULL,
  draws = 1000, seed = 20260807)
estimate_process_uncertainty(x, spec = process_uncertainty_spec(), metrics = NULL,
  cluster = NULL)
propagate_process_uncertainty(x, estimand = function(data) mean(data, na.rm = TRUE),
  method = c("bootstrap", "simulation", "posterior"), draws = NULL, seed = NULL)
uncertainty_budget(x)
compare_uncertainty_budgets(...)
plot_uncertainty_waterfall(x, ...)
plot_uncertainty_tornado(x, ...)
plot_uncertainty_by_item(x, ...)
plot_uncertainty_by_stage(x, ...)
```

## Arguments

- calibration:

  Argument controlling \`calibration\`; see the function usage and
  returned audit metadata.

- aoi_assignment:

  Argument controlling \`aoi_assignment\`; see the function usage and
  returned audit metadata.

- preprocessing:

  Argument controlling \`preprocessing\`; see the function usage and
  returned audit metadata.

- sampling:

  Argument controlling \`sampling\`; see the function usage and returned
  audit metadata.

- model:

  Argument controlling \`model\`; see the function usage and returned
  audit metadata.

- source_sd:

  Argument controlling \`source_sd\`; see the function usage and
  returned audit metadata.

- draws:

  Argument controlling \`draws\`; see the function usage and returned
  audit metadata.

- seed:

  Argument controlling \`seed\`; see the function usage and returned
  audit metadata.

- x:

  Input object or data structure appropriate for the selected analysis.

- spec:

  Argument controlling \`spec\`; see the function usage and returned
  audit metadata.

- metrics:

  Argument controlling \`metrics\`; see the function usage and returned
  audit metadata.

- cluster:

  Argument controlling \`cluster\`; see the function usage and returned
  audit metadata.

- estimand:

  Argument controlling \`estimand\`; see the function usage and returned
  audit metadata.

- method:

  Argument controlling \`method\`; see the function usage and returned
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
