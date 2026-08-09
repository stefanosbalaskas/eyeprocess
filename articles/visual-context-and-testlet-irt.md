# Visual-context and testlet IRT

Items displayed on the same screen, page, diagram, or stimulus can share
presentation-context variance.
[`fit_visual_context_irt()`](https://stefanosbalaskas.github.io/eyeprocess/reference/fit_visual_context_irt.md)
models that known design dependence as a separate testlet/context
dimension rather than silently treating every item as independent.

``` r

registry <- visual_context_registry(
  item_metadata,
  item = "item_id",
  context = "screen_id"
)

fit <- fit_visual_context_irt(
  response_matrix,
  registry = registry,
  context = "screen_3"
)

compare_visual_context_irt(fit)
context_factor_effects(fit)
audit_visual_context_dependence(fit)
plot(fit, type = "loadings")
plot(fit, type = "difficulty_change")
plot(fit, type = "context_registry")
```

The context dimension represents shared presentation context unless
independent substantive theory and validation justify another
interpretation.
