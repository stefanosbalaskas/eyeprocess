# Audit basic multimodal design identifiability

This is a structural pre-flight screen, not a proof of statistical
identifiability.

## Usage

``` r
audit_multimodal_identifiability(x, min_person = 30L, min_item = 5L)
```

## Arguments

- x:

  An \`eye_multimodal_measurement\`.

- min_person, min_item:

  Minimum structural counts.

## Value

An \`eye_multimodal_identifiability_audit\`.
