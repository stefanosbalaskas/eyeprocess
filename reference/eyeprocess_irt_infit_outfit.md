# Compute residual-based Infit and Outfit summaries

These statistics summarize response-model residual behavior. They are
not labels for motivation, misconduct, diagnosis, or respondent intent.

## Usage

``` r
eyeprocess_irt_infit_outfit(
  observed,
  expected,
  by = c("item", "person"),
  min_variance = 1e-08
)
```

## Arguments

- observed:

  Observed responses or observed values.

- expected:

  Model-expected probabilities or expected values.

- by:

  Grouping variables or aggregation level.

- min_variance:

  Minimum variance used to stabilize residual calculations.
