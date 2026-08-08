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
