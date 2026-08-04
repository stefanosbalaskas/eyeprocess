# Fit a theory-defined strategy-informed IRT model

Strategy prototypes are declared before estimation. Posterior strategy
probabilities are computed from Gaussian feature-distance likelihoods
and entered into a response model with person and item effects.

## Usage

``` r
fit_theory_strategy_irt(
  x,
  spec,
  response = "score",
  participant = "participant_id",
  item = "item_id"
)
```

## Arguments

- x:

  An \`eye_dataset\` or model data frame.

- spec:

  Theory strategy specification.

- response:

  Response column.

- participant:

  Participant column.

- item:

  Item column.

## Value

An \`eye_theory_strategy_irt\` object.
