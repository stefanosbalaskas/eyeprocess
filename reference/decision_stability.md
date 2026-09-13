# Overall decision-stability summary

Overall decision-stability summary

## Usage

``` r
decision_stability(
  x,
  effect = "effect",
  p_value = NULL,
  alpha = 0.05,
  threshold = 0
)
```

## Arguments

- x:

  Sensitivity result.

- effect:

  Effect column.

- p_value:

  Optional p-value column.

- alpha:

  Significance threshold.

- threshold:

  Substantive threshold.

## Value

An object of class "eye_decision_stability", stored as a named list,
with components "summary", "stable_sign", "stable_threshold",
"stable_significance", "thresholds", "caveat". It contains overall
decision-stability summary and associated metadata or diagnostics needed
to interpret the result.
