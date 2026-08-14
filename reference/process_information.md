# Quantify incremental process information

Compares posterior draws for the same latent target under a baseline and
an augmented model. This implementation intentionally uses posterior
uncertainty metrics rather than assuming Fisher information is
additively decomposable across heterogeneous channels.

## Usage

``` r
process_information(
  baseline,
  augmented,
  metric = c("variance_reduction", "precision_gain", "entropy_reduction")
)
```

## Arguments

- baseline, augmented:

  Numeric posterior draws. Rows are draws and columns are matched latent
  targets.

- metric:

  \`variance_reduction\`, \`precision_gain\`, or \`entropy_reduction\`.

## Value

An \`eye_process_information\` data.frame.
