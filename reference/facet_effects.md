# Extract facet effects from a many-facet process model

Extract facet effects from a many-facet process model

## Usage

``` r
facet_effects(object, channel = c("response", "process"))
```

## Arguments

- object:

  A fitted eyeprocess model or audit object.

- channel:

  Measurement channel to inspect.

## Value

A named list with components "random_effects", "variance_components",
containing facet effects from a many-facet process model and associated
metadata or diagnostics.
