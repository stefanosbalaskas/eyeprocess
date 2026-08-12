# Iteratively remove anchors exceeding a supplied effect threshold

Iteratively remove anchors exceeding a supplied effect threshold

## Usage

``` r
eyeprocess_irt_anchor_purification(
  items,
  effect_fun,
  initial = items$item_id,
  threshold = 0.1,
  max_iter = 10L
)
```

## Arguments

- items:

  Item-parameter data frame or item collection.

- effect_fun:

  Function that computes the anchor-screening effect.

- initial:

  Initial value, state, or anchor set.

- threshold:

  Decision or diagnostic threshold.

- max_iter:

  Maximum number of iterations.
