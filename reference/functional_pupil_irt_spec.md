# Specify a functional pupil-IRT model

Specify a functional pupil-IRT model

## Usage

``` r
functional_pupil_irt_spec(
  df = 5L,
  response = "score",
  engine = c("two_stage_glm", "two_stage_lme4", "brms"),
  include_response_time = TRUE
)
```

## Arguments

- df:

  Natural-spline degrees of freedom.

- response:

  Response field.

- engine:

  Two-stage GLM/multilevel engine or joint \`brms\` engine.

- include_response_time:

  Include a response-time submodel when joint.

## Value

An \`eye_functional_pupil_irt_spec\`.
