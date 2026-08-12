# Create a governed IRT model card

Create a governed IRT model card

## Usage

``` r
eyeprocess_irt_model_card(
  spec,
  engine_status = NULL,
  identification = NULL,
  fit_evidence = NULL,
  invariance = NULL,
  validation = NULL,
  intended_use = NULL,
  excluded_interpretations = c("diagnosis", "cheating inference",
    "mental-state inference")
)
```

## Arguments

- spec:

  Model, validation, or analysis specification object.

- engine_status:

  Availability/status record for the selected estimation engine.

- identification:

  Identification specification or identification audit.

- fit_evidence:

  Model-fit evidence or diagnostics.

- invariance:

  Measurement-invariance evidence or audit.

- validation:

  Value supplied for the validation argument.

- intended_use:

  Statement of the intended analytical use.

- excluded_interpretations:

  Interpretations explicitly excluded by the model card.
