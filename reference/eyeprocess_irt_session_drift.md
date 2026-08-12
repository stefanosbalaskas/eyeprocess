# Summarise item-parameter drift over sessions

Summarise item-parameter drift over sessions

## Usage

``` r
eyeprocess_irt_session_drift(
  parameters,
  item_id = "item_id",
  session = "session",
  parameter = "b"
)
```

## Arguments

- parameters:

  Item-parameter data frame or parameter estimates.

- item_id:

  Item identifier or vector of item identifiers.

- session:

  Session identifier or grouping variable.

- parameter:

  Name of the parameter to compare.
