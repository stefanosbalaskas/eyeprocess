# Binary/ordinal response channel for multimodal IRT

Binary/ordinal response channel for multimodal IRT

## Usage

``` r
irt_response_channel(
  family = c("2pl", "rasch", "graded", "partial_credit"),
  response = "response",
  latent = "ability",
  options = list()
)
```

## Arguments

- family:

  Statistical family used by the channel or model.

- response:

  Response variable or response-column name.

- latent:

  Latent variable or latent-variable labels.

- options:

  Additional channel/model options.

## Value

A named list with components "type", "family", "role", "link",
"variables", "latent", "options", containing binary/ordinal response
channel for multimodal IRT and associated metadata or diagnostics.
