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
