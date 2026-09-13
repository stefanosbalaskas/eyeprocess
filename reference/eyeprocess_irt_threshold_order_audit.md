# Audit ordered category thresholds

Audit ordered category thresholds

## Usage

``` r
eyeprocess_irt_threshold_order_audit(item_id, thresholds)
```

## Arguments

- item_id:

  Item identifier or vector of item identifiers.

- thresholds:

  Ordered response-category thresholds.

## Value

An object of class "eye_irt_threshold_audit", stored as a named list,
with components "item_id", "thresholds", "ordered", "minimum_gap",
"reversals". It contains ordered category thresholds and associated
metadata or diagnostics needed to interpret the result.
