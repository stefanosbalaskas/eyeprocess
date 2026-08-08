# Define a multimodal IRT model specification

Define a multimodal IRT model specification

## Usage

``` r
irt_model_spec(
  id,
  latent,
  channels,
  status = c("experimental", "reference", "gated"),
  fit_fun = NULL,
  simulate_fun = NULL,
  validate_fun = NULL,
  citation = character(),
  description = NULL,
  requirements = character(),
  metadata = list()
)
```

## Arguments

- id:

  Stable model identifier.

- latent:

  Named or unnamed latent dimensions.

- channels:

  Named list of channel objects.

- status:

  One of reference, experimental, or gated.

- fit_fun:

  Optional fitting function.

- simulate_fun:

  Optional simulator.

- validate_fun:

  Optional model-specific validation function.

- citation:

  Character vector of citations/DOIs.

- description:

  Human-readable model description.

- requirements:

  Optional packages/engines.

- metadata:

  Additional metadata.

## Value

An \`eye_irt_model_spec\`.
