# Specify a process-deployment drift audit

Specify a process-deployment drift audit

## Usage

``` r
process_drift_spec(
  baseline = c("first_batch", "reference_batch"),
  difficulty_limit = 0.4,
  discrimination_limit = 0.35,
  gaze_validity_drop = 0.1,
  luminance_limit = 25,
  relative_metric_quantile = 0.9,
  min_batches = 2L
)
```

## Arguments

- baseline:

  Baseline rule: first observed batch or an explicitly supplied
  reference batch.

- difficulty_limit:

  Absolute item-difficulty change triggering review.

- discrimination_limit:

  Absolute discrimination change triggering review.

- gaze_validity_drop:

  Maximum tolerated decrease in gaze validity.

- luminance_limit:

  Absolute luminance change triggering review.

- relative_metric_quantile:

  Quantile of absolute deltas used for process metrics without an
  externally meaningful absolute threshold.

- min_batches:

  Minimum number of batches per item for drift assessment.

## Value

An \`eye_process_drift_spec\` object.
