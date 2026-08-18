# Audit structural and measurement support for M3

Performs a conservative pre-fit audit for the four-channel response, RT,
fixation-count and pupil reference model. The audit checks design
connectivity, channel coverage and variation, pupil nuisance
availability, blink/interpolation burden, device/session representation
and explicit M3 identification constraints. It is a support screen, not
proof of global identifiability or construct validity.

## Usage

``` r
audit_multimodal_m3_identifiability(
  x,
  pupil_scale = c("z", "raw"),
  min_persons = 20L,
  min_items = 5L,
  max_pupil_missing = 0.5,
  max_blink_rate = 0.3,
  max_interpolation_rate = 0.3
)
```

## Arguments

- x:

  M3-compatible data, simulation, fit, or measurement object.

- pupil_scale:

  Pupil transformation used by the reference likelihood.

- min_persons, min_items:

  Conservative design thresholds.

- max_pupil_missing, max_blink_rate, max_interpolation_rate:

  Warning thresholds.

## Value

An \`eye_multimodal_m3_identifiability\` object.
