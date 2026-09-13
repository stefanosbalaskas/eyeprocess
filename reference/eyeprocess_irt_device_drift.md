# Summarise parameter drift across acquisition devices

Summarise parameter drift across acquisition devices

## Usage

``` r
eyeprocess_irt_device_drift(
  parameters,
  item_id = "item_id",
  device = "device",
  parameter = "b"
)
```

## Arguments

- parameters:

  Item-parameter data frame or parameter estimates.

- item_id:

  Item identifier or vector of item identifiers.

- device:

  Device identifier or grouping variable.

- parameter:

  Name of the parameter to compare.

## Value

A tabular R object containing parameter drift across acquisition
devices; rows represent analysis units and columns contain the returned
quantities.
