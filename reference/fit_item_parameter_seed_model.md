# Fit an experimental pre-pilot item-parameter seeding model

Estimates screening predictions for item difficulty/discrimination from
item design/process features. Predictions are not calibrated operational
parameters.

## Usage

``` r
fit_item_parameter_seed_model(
  item_data,
  difficulty = "irt_difficulty",
  discrimination = "irt_discrimination",
  predictors,
  engine = c("auto", "ranger", "lm"),
  seed = 2221
)
```

## Arguments

- item_data:

  Calibrated item-level training data.

- difficulty, discrimination:

  Target columns.

- predictors:

  Design/process predictors.

- engine:

  \`auto\`, \`ranger\`, or \`lm\`.

- seed:

  Seed.
