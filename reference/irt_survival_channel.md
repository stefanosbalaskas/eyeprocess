# Survival/event-time channel for multimodal IRT

Survival/event-time channel for multimodal IRT

## Usage

``` r
irt_survival_channel(
  family = c("cox", "weibull", "exponential"),
  time = "time",
  event = "event",
  latent = NULL,
  options = list()
)
```

## Arguments

- family:

  Statistical family used by the channel or model.

- time:

  Time values.

- event:

  Event indicator or event column.

- latent:

  Latent variable or latent-variable labels.

- options:

  Additional channel/model options.

## Value

A named list with components "type", "family", "role", "link",
"variables", "latent", "options", containing survival/event-time channel
for multimodal IRT and associated metadata or diagnostics.
