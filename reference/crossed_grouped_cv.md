# Cross-classified grouped cross-validation

Cross-classified grouped cross-validation

## Usage

``` r
crossed_grouped_cv(
  data,
  formula,
  family = stats::binomial(),
  groups = c("participant_id", "item_id"),
  v = 5L,
  metric = c("log_loss", "brier", "accuracy"),
  seed = 1L
)
```

## Arguments

- data:

  Data frame.

- formula:

  Model formula.

- family:

  GLM family.

- groups:

  Crossed grouping columns.

- v:

  Number of folds.

- metric:

  Metric: log loss, Brier score, or accuracy.

- seed:

  Random seed.

## Value

An \`eye_crossed_grouped_cv\` object.
