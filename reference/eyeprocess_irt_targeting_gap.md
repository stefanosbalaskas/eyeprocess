# Compare an examinee distribution with item-bank targeting

Compare an examinee distribution with item-bank targeting

## Usage

``` r
eyeprocess_irt_targeting_gap(theta, items, breaks = seq(-4, 4, by = 0.5))
```

## Arguments

- theta:

  Latent-trait value or vector of latent-trait values.

- items:

  Item-parameter data frame or item collection.

- breaks:

  Break points used to summarize latent-scale targeting.

## Value

An object of class "eye_irt_targeting_gap", stored as a named list, with
components "table", "absolute_gap", "interpretation". It contains an
examinee distribution with item-bank targeting and associated metadata
or diagnostics needed to interpret the result.
