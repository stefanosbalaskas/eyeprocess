# Evaluate a simple adaptive stopping rule

Evaluate a simple adaptive stopping rule

## Usage

``` r
eyeprocess_irt_stopping_rule(
  n_administered,
  se = NA_real_,
  min_items = 5L,
  max_items = 30L,
  target_se = 0.3
)
```

## Arguments

- n_administered:

  Number of items already administered.

- se:

  Standard-error values.

- min_items:

  Minimum number of items required.

- max_items:

  Maximum permitted test length.

- target_se:

  Target conditional standard error for stopping.
