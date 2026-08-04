# Specify a gaze-informed diffusion workflow

Specify a gaze-informed diffusion workflow

## Usage

``` r
gaze_diffusion_spec(
  engine = c("ez_regression", "diffIRT", "brms"),
  gaze_features = character(),
  response = "score",
  response_time = "response_time"
)
```

## Arguments

- engine:

  Approximate EZ regression, \`diffIRT\`, or Bayesian Wiener model.

- gaze_features:

  Process predictors.

- response:

  Accuracy field.

- response_time:

  Response-time field.

## Value

An \`eye_gaze_diffusion_spec\`.
