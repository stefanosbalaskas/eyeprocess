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
