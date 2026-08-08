# Ablate process channels under a common out-of-sample evaluator

Ablate process channels under a common out-of-sample evaluator

## Usage

``` r
process_channel_ablation(
  data,
  channels,
  evaluator,
  baseline = character(),
  higher_is_better = TRUE
)
```

## Arguments

- data:

  Input data.

- channels:

  Named list whose elements are character vectors of columns.

- evaluator:

  Function \`(data, active_columns, channel_name)\` returning a scalar
  out-of-sample score. The evaluator owns all fitting/splitting logic.

- baseline:

  Character vector of always-active columns.

- higher_is_better:

  Direction of the score.
