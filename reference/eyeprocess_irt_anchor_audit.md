# Audit candidate anchor items using supplied DIF evidence

Audit candidate anchor items using supplied DIF evidence

## Usage

``` r
eyeprocess_irt_anchor_audit(
  items,
  dif = NULL,
  max_abs_effect = 0.1,
  min_information = NULL
)
```

## Arguments

- items:

  Item-parameter data frame or item collection.

- dif:

  Differential-item-functioning evidence or summary.

- max_abs_effect:

  Maximum permitted absolute effect for an anchor candidate.

- min_information:

  Minimum required item information.
