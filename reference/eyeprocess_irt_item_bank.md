# Item bank object for adaptive design

Item bank object for adaptive design

## Usage

``` r
eyeprocess_irt_item_bank(items, content = NULL, exposure_limit = 1)
```

## Arguments

- items:

  Item-parameter data frame or item collection.

- content:

  Item content/category metadata.

- exposure_limit:

  Maximum permitted item exposure.

## Value

An object of class "eye_irt_item_bank", stored as a named list, with
components "items", "exposure_limit". It contains item bank object for
adaptive design and associated metadata or diagnostics needed to
interpret the result.
