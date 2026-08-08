# Fit a multiple-response process-IRT reference model

The bundled reference treats option selections as repeated binary
outcomes with option-specific random difficulty/discrimination and
optional gaze. This preserves option-level observations but does
\*\*not\*\* reproduce the 2026 MRM/MRM-LD likelihood. Use \`engine =
"external"\` for a validated exact implementation of a multiple-response
model with inter-option dependence.

## Usage

``` r
fit_multiple_response_process_irt(
  data,
  selected = "selected",
  theta = "theta",
  person = "participant_id",
  item = "item_id",
  option = "option_id",
  gaze = NULL,
  engine = c("reference", "external"),
  external_engine = NULL,
  ...
)
```

## Arguments

- data:

  Long person-item-option table.

- selected:

  Binary option-selection indicator.

- theta:

  Supplied latent-trait estimate/score.

- person, item, option:

  Identifiers.

- gaze:

  Optional option-level process measure.

- engine:

  \`reference\` or \`external\`.

- external_engine:

  Validated external fitter.

- ...:

  Arguments passed to the external engine.
