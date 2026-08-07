# Measurement-intelligence plotting and result infrastructure

Measurement-intelligence plotting and result infrastructure. These
functions form the eyeprocess 0.6.0.9000 measurement-intelligence
programme and use dependency-free reference implementations with
explicit evidence limits.

## Usage

``` r
eye_plot_spec(type = "default", title = NULL, xlab = NULL, ylab = NULL,
  caption = NULL, show_uncertainty = TRUE, show_raw = TRUE, facet_by = NULL,
  label_items = FALSE, interactive = FALSE)
plot_diagnostics(x, ...)
plot_evidence(x, ...)
plot_sensitivity(x, ...)
autoplot_eyeprocess(object, ...)
```

## Arguments

- type:

  Argument controlling \`type\`; see the function usage and returned
  audit metadata.

- title:

  Argument controlling \`title\`; see the function usage and returned
  audit metadata.

- xlab:

  Argument controlling \`xlab\`; see the function usage and returned
  audit metadata.

- ylab:

  Argument controlling \`ylab\`; see the function usage and returned
  audit metadata.

- caption:

  Argument controlling \`caption\`; see the function usage and returned
  audit metadata.

- show_uncertainty:

  Argument controlling \`show_uncertainty\`; see the function usage and
  returned audit metadata.

- show_raw:

  Argument controlling \`show_raw\`; see the function usage and returned
  audit metadata.

- facet_by:

  Argument controlling \`facet_by\`; see the function usage and returned
  audit metadata.

- label_items:

  Argument controlling \`label_items\`; see the function usage and
  returned audit metadata.

- interactive:

  Argument controlling \`interactive\`; see the function usage and
  returned audit metadata.

- x:

  Input object or data structure appropriate for the selected analysis.

- ...:

  Additional arguments passed to the underlying method or plotting
  function.

- object:

  Input object or data structure appropriate for the selected analysis.

## Details

The APIs return auditable S3 objects. Plot wrappers call registered
base-graphics methods. Experimental or approximate engines are labelled
in object status fields and should be validated before confirmatory or
operational use.

## Value

An eyeprocess result object, data frame, model object, plot, or audit
table as documented by the individual function.

## See also

`plot_diagnostics()`, `plot_evidence()`, and `plot_sensitivity()`.
