# Pupil phase-amplitude registration

Pupil phase-amplitude registration. These functions form the eyeprocess
0.6.0.9000 measurement-intelligence programme and use dependency-free
reference implementations with explicit evidence limits.

## Usage

``` r
register_pupil_curves(x, time, pupil, anchor = c("stimulus", "response", "event"),
  method = c("elastic", "landmark"), id_col = "person_id", grid_size = 101)
decompose_pupil_phase_amplitude(x, components = 3)
fit_phase_amplitude_irt(responses, phase_scores, amplitude_scores = NULL,
  person_id = NULL, family = c("gaussian", "binomial"), ...)
audit_pupil_registration(x)
plot_pupil_registration(x, ...)
plot_warping_functions(x, ...)
plot_phase_amplitude_scores(x, ...)
plot_item_phase_delay(x, ...)
plot_registered_pupil_effects(x, ...)
```

## Arguments

- x:

  Input object or data structure appropriate for the selected analysis.

- time:

  Argument controlling \`time\`; see the function usage and returned
  audit metadata.

- pupil:

  Argument controlling \`pupil\`; see the function usage and returned
  audit metadata.

- anchor:

  Argument controlling \`anchor\`; see the function usage and returned
  audit metadata.

- method:

  Argument controlling \`method\`; see the function usage and returned
  audit metadata.

- id_col:

  Argument controlling \`id_col\`; see the function usage and returned
  audit metadata.

- grid_size:

  Argument controlling \`grid_size\`; see the function usage and
  returned audit metadata.

- components:

  Argument controlling \`components\`; see the function usage and
  returned audit metadata.

- responses:

  Argument controlling \`responses\`; see the function usage and
  returned audit metadata.

- phase_scores:

  Argument controlling \`phase_scores\`; see the function usage and
  returned audit metadata.

- amplitude_scores:

  Argument controlling \`amplitude_scores\`; see the function usage and
  returned audit metadata.

- person_id:

  Argument controlling \`person_id\`; see the function usage and
  returned audit metadata.

- family:

  Argument controlling \`family\`; see the function usage and returned
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
