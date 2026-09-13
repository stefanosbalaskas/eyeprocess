# Run stepwise Rasch item-reduction as a sensitivity analysis

Run stepwise Rasch item-reduction as a sensitivity analysis

## Usage

``` r
audit_item_reduction_sensitivity(
  erm_model,
  criterion = list("itemfit"),
  alpha = 0.05,
  maxstep = 5L
)
```

## Arguments

- erm_model:

  Fitted eRm model.

- criterion:

  Criterion list passed to \`eRm::stepwiseIt()\`.

- alpha:

  Significance threshold.

- maxstep:

  Maximum elimination steps.

## Value

An object of class "eye_item_reduction_sensitivity", stored as a named
list, with components "model", "eliminated_items", "alpha", "maxstep",
"status", "caveat". It contains stepwise Rasch item-reduction as a
sensitivity analysis and associated metadata or diagnostics needed to
interpret the result.
