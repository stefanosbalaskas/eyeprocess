# Compositional AOI channel

Compositional AOI channel

## Usage

``` r
irt_compositional_channel(
  parts,
  family = c("logratio_gaussian", "dirichlet"),
  latent = "process",
  options = list()
)
```

## Arguments

- parts:

  Compositional parts.

- family:

  Statistical family used by the channel or model.

- latent:

  Latent variable or latent-variable labels.

- options:

  Additional channel/model options.

## Value

A named list with components "type", "family", "role", "link",
"variables", "latent", "options", containing compositional AOI channel
and associated metadata or diagnostics.
