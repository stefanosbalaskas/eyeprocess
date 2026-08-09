# Fit a luminance/fatigue/process confound model for pupil response

Fit a luminance/fatigue/process confound model for pupil response

## Usage

``` r
fit_pupil_confound_model(
  data,
  pupil = "pupil_peak",
  luminance = "screen_luminance",
  trial_order = "trial_sequence",
  theta = NULL,
  person = "person_id",
  item = "item_id",
  engine = c("auto", "mgcv", "lm")
)
```

## Arguments

- data:

  Trial-level data.

- pupil, luminance, trial_order:

  Column names.

- theta:

  Optional latent-score column.

- person, item:

  Optional identifiers.

- engine:

  \`auto\`, \`mgcv\`, or \`lm\`.

## Value

An \`eye_pupil_confound_model\` object containing raw and adjusted
values.
