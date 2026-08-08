# Promote an IRT model after evidence gates are met

Promotion is an evidence record, not a mutable global status change
unless \`update_registry = TRUE\` is requested.

## Usage

``` r
promote_irt_model(
  spec,
  evidence,
  target = c("experimental", "reference"),
  update_registry = FALSE
)
```

## Arguments

- spec:

  IRT model or validation specification.

- evidence:

  Validation evidence used for promotion.

- target:

  Target evidence/status level.

- update_registry:

  Whether the in-memory registry is updated.
