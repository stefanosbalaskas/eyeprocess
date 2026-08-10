# Declare temporal provenance for process features

Declare temporal provenance for process features

## Usage

``` r
process_feature_time_provenance(
  feature,
  available_at,
  outcome_at,
  source = NA_character_,
  transformation = NA_character_,
  unit = "ms"
)
```

## Arguments

- feature:

  Feature names.

- available_at:

  Earliest time at which each feature is available.

- outcome_at:

  Time at which the modeled outcome becomes available.

- source:

  Optional source labels.

- transformation:

  Optional transformation descriptions.

- unit:

  Time unit label.

## Value

A provenance table.
