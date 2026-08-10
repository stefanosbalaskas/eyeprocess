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
