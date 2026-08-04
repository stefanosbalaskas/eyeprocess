# Evaluate a model with grouped cross-validation

Evaluate a model with grouped cross-validation

## Usage

``` r
grouped_cv(
  data,
  formula,
  family = stats::binomial(),
  group = "participant_id",
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

- group:

  Grouping columns.

- v:

  Number of folds.

- metric:

  Metric: log loss, Brier score, or accuracy.

- seed:

  Random seed.

## Value

An \`eye_grouped_cv\` object.
