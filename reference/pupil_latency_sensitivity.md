# Pupil latency estimator sensitivity and resolvability audit

Estimates pupil-response latency using a sustained threshold,
maximum-slope tangent intersection, and piecewise breakpoint. The result
reports estimator spread and a sampling/noise-aware resolvability label
rather than treating a single latency estimate as algorithm- or
hardware-independent.

## Usage

``` r
pupil_latency_sensitivity(
  time,
  pupil,
  event_time = 0,
  baseline_window = c(-0.5, 0),
  search_window = c(0, 2),
  direction = c("constriction", "dilation"),
  threshold_sigma = 3,
  sustain_ms = 40
)
```

## Arguments

- time:

  Numeric sample times in seconds.

- pupil:

  Numeric pupil values.

- event_time:

  Nominal event time in seconds.

- baseline_window:

  Two-element window relative to \`event_time\`.

- search_window:

  Two-element search window relative to \`event_time\`.

- direction:

  \`"constriction"\` or \`"dilation"\`.

- threshold_sigma:

  Robust-noise multiples used by the sustained threshold.

- sustain_ms:

  Required duration above threshold in milliseconds.

## Value

A list of estimator-specific latencies, spread, signal diagnostics,
resolvability, and provenance.
