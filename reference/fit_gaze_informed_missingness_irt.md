# Fit a gaze-informed missingness IRT diagnostic

Fits a transparent two-part reference model: (1) whether an item
response is missing and (2) the observed response, both conditional on a
supplied latent trait (or a clearly labelled person-score proxy), item,
and visual exposure. This is a diagnostic bridge to joint MNAR/process
IRT, not a substitute for a fully joint latent missingness model.

## Usage

``` r
fit_gaze_informed_missingness_irt(
  data,
  response = "response",
  person = "participant_id",
  item = "item_id",
  gaze_exposure = "gaze_exposure",
  theta = NULL,
  reached = NULL
)
```

## Arguments

- data:

  Long person-item data.

- response:

  Response column; missing values identify omissions.

- person, item:

  Person and item identifiers.

- gaze_exposure:

  Non-negative visual-exposure measure.

- theta:

  Optional latent-trait column. If \`NULL\`, a smoothed person
  proportion-correct logit is used as an explicit proxy.

- reached:

  Optional reached/not-reached indicator.

## Value

An \`eye_gaze_informed_missingness_irt\` object.
