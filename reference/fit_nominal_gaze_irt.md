# Nominal/distractor IRT with option-level gaze

The bundled estimator is a transparent two-stage process-augmented
nominal model: participant ability may be supplied, or a shrinkage logit
accuracy proxy is estimated; option-level gaze proportions then enter a
multinomial response model. This is intended for validation and
exploratory distractor research, not as a replacement for a fully latent
nominal-response model.

## Usage

``` r
fit_nominal_gaze_irt(
  data,
  response_option = "response_option",
  option_gaze,
  person = "participant_id",
  item = "item_id",
  ability = NULL,
  correct_option = NULL,
  add_item_effects = TRUE,
  ...
)
```

## Arguments

- data:

  Input data frame or compatible tabular object.

- response_option:

  Column identifying the selected response option.

- option_gaze:

  Character vector naming one gaze column per response option. Names
  should correspond to option labels when possible.

- person:

  Person or participant identifier column.

- item:

  Item identifier, name, or item column.

- ability:

  Optional existing ability score column.

- correct_option:

  Optional scalar or column name identifying correct option.

- add_item_effects:

  Whether item effects are included.

- ...:

  Additional arguments passed to the selected model, engine, or method.
