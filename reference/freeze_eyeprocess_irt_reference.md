# Freeze IRT validation reference summaries

Freeze IRT validation reference summaries

## Usage

``` r
freeze_eyeprocess_irt_reference(
  recovery_summary = NULL,
  sbc = NULL,
  failures = NULL,
  metadata = list()
)
```

## Arguments

- recovery_summary:

  Parameter-recovery summary object or table.

- sbc:

  Simulation-based-calibration evidence object or table.

- failures:

  Failure records or failure summary.

- metadata:

  Named metadata to store with the frozen object.

## Value

A named list with components "recovery_summary", "sbc", "failures",
"metadata", "scientific_scope", containing freeze IRT validation
reference summaries and associated metadata or diagnostics.
