# Extract session facet effects

Extract session facet effects

## Usage

``` r
session_facet_effects(object, channel = c("response", "process"))
```

## Arguments

- object:

  A fitted eyeprocess model or audit object.

- channel:

  Measurement channel to inspect.

## Value

An object of class "eye_process_facet_effects", stored as a named list,
with components "facet", "column", "channel", "random_effects",
"variance_component". It contains session facet effects and associated
metadata or diagnostics needed to interpret the result.
