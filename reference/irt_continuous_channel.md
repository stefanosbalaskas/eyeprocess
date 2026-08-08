# Continuous/bounded process channel for multimodal IRT

Continuous/bounded process channel for multimodal IRT

## Usage

``` r
irt_continuous_channel(
  family = c("censored_normal", "beta", "gaussian"),
  value = "process_value",
  lower = 0,
  upper = 1,
  latent = "process",
  options = list()
)
```

## Arguments

- family:

  Continuous response family.

- value:

  Variable name.

- lower, upper:

  Bounds where applicable.

- latent:

  Latent dimension.

- options:

  Additional channel metadata.
