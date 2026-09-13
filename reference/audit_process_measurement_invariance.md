# Audit process measurement invariance across facets

Audit process measurement invariance across facets

## Usage

``` r
audit_process_measurement_invariance(
  object,
  channel = c("process", "response"),
  relative_sd_threshold = 0.25
)
```

## Arguments

- object:

  A fitted eyeprocess model or audit object.

- channel:

  Measurement channel to inspect.

- relative_sd_threshold:

  Value supplied to \`relative_sd_threshold\`; see Details for its
  model-specific role.

## Value

An object of class "eye_process_measurement_invariance", stored as a
named list, with components "pass", "threshold", "components",
"channel", "note". It contains process measurement invariance across
facets and associated metadata or diagnostics needed to interpret the
result.
