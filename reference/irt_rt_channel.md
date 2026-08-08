# Response-time channel for multimodal IRT

Response-time channel for multimodal IRT

## Usage

``` r
irt_rt_channel(
  family = c("lognormal", "gaussian_log", "shifted_lognormal"),
  rt = "rt",
  latent = "speed",
  options = list()
)
```

## Arguments

- family:

  Statistical family used by the channel or model.

- rt:

  Response-time variable or column name.

- latent:

  Latent variable or latent-variable labels.

- options:

  Additional channel/model options.
