# Validate latent-space proximity against process similarity

Validate latent-space proximity against process similarity

## Usage

``` r
validate_latent_space_process_similarity(
  object,
  process_matrix,
  entity = c("person", "item")
)
```

## Arguments

- object:

  A fitted eyeprocess model or audit object.

- process_matrix:

  Rows correspond to persons or items in the same order as the fitted
  latent coordinates.

- entity:

  Entity type to map or validate.

## Value

An object of class "eye_latent_space_process_validation", stored as a
named list, with components "entity", "spearman_distance_correlation",
"latent_distance", "process_distance", "interpretation". It contains
latent-space proximity against process similarity and associated
metadata or diagnostics needed to interpret the result.
