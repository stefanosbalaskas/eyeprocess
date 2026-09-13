# Audit declared order of pupil preprocessing steps

Audit declared order of pupil preprocessing steps

## Usage

``` r
audit_pupil_preprocessing_order(
  steps,
  cleaning_patterns = c("blink", "missing", "interpol", "artifact", "smooth", "filter"),
  baseline_pattern = "baseline"
)
```

## Arguments

- steps:

  Character vector in execution order.

- cleaning_patterns:

  Patterns considered cleaning/preprocessing.

- baseline_pattern:

  Pattern identifying baseline correction.

## Value

A named list with components "steps", "baseline_positions",
"cleaning_positions", "cleaning_after_baseline", "status", "caveat",
containing declared order of pupil preprocessing steps and associated
metadata or diagnostics.
