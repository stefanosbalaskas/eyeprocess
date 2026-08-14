# Consolidated multimodal IRT specification

This is a thin multimodal adapter over the established
\`irt_model_spec()\` / \`eye_irt_model_spec\` architecture. It composes
existing \`eye_irt_channel\` objects and does not define a parallel
channel or model-specification ecosystem.

## Usage

``` r
multimodal_irt_spec(
  response = NULL,
  rt = NULL,
  gaze = NULL,
  pupil = NULL,
  model = c("M0", "M1", "M2", "M3"),
  backend = c("cmdstanr", "existing"),
  identification = list(),
  priors = list()
)
```

## Arguments

- response, rt, gaze, pupil:

  Existing \`eye_irt_channel\` objects or NULL.

- model:

  Development model identifier.

- backend:

  Requested backend.

- identification:

  Named identification settings retained as explicit multimodal
  metadata.

- priors:

  Named prior settings retained as explicit multimodal metadata.

## Value

An \`eye_multimodal_irt_spec\` convenience subclass of the established
\`eye_irt_model_spec\`.
