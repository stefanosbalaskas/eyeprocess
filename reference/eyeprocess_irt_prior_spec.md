# Declare prior families for Bayesian IRT engine adapters

Declare prior families for Bayesian IRT engine adapters

## Usage

``` r
eyeprocess_irt_prior_spec(
  discrimination = c("lognormal", "normal"),
  difficulty = "normal",
  guessing = c("beta", "logit-normal"),
  location = 0,
  scale = 1,
  guessing_shape = c(5, 17),
  label = "default"
)
```

## Arguments

- discrimination:

  Discrimination vector or matrix.

- difficulty:

  Item difficulty or location parameter.

- guessing:

  Prior distribution family for the guessing parameter.

- location:

  Prior location hyperparameter.

- scale:

  Prior scale hyperparameter.

- guessing_shape:

  Shape parameters for the guessing prior.

- label:

  Human-readable label.
