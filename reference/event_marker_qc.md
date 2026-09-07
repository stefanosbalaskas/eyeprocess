# Event-marker plausibility audit

Evaluates whether independent channel offsets corroborate a nominal
event. This is annotation/event plausibility QC, not clock
synchronization, and it never modifies timestamps.

## Usage

``` r
event_marker_qc(offsets, tolerance, min_corroborating = 2L)
```

## Arguments

- offsets:

  Numeric corroborating-channel offsets in seconds.

- tolerance:

  Allowed absolute offset in seconds.

- min_corroborating:

  Minimum corroborating channels for \`confirmed\`.

## Value

A list containing status, consensus offset, uncertainty, and counts.
