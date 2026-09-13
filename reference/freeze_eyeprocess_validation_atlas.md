# Freeze a validation atlas with a reproducibility fingerprint

Freeze a validation atlas with a reproducibility fingerprint

## Usage

``` r
freeze_eyeprocess_validation_atlas(atlas, metadata = list())
```

## Arguments

- atlas:

  Validation-evidence atlas object.

- metadata:

  Named metadata to store with the frozen object.

## Value

An object of class "eye_validation_atlas_freeze", stored as a named
list, with components "payload", "hash", "frozen". It contains freeze a
validation atlas with a reproducibility fingerprint and associated
metadata or diagnostics needed to interpret the result.
