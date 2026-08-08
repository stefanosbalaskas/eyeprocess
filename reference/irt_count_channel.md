# Count-valued process channel for multimodal IRT

Count-valued process channel for multimodal IRT

## Usage

``` r
irt_count_channel(
  family = c("negative_binomial", "poisson"),
  value = "fixation_count",
  latent = "engagement",
  options = list()
)
```

## Arguments

- family:

  Statistical family used by the channel or model.

- value:

  Process-value column or values.

- latent:

  Latent variable or latent-variable labels.

- options:

  Additional channel/model options.
