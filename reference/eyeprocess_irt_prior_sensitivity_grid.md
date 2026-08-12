# Construct a prior-sensitivity grid for Bayesian IRT analyses

Construct a prior-sensitivity grid for Bayesian IRT analyses

## Usage

``` r
eyeprocess_irt_prior_sensitivity_grid(
  discrimination_scale = c(0.5, 1, 1.5),
  difficulty_scale = c(1, 2),
  guessing_mean = c(0.1, 0.2)
)
```

## Arguments

- discrimination_scale:

  Prior scale for item discrimination.

- difficulty_scale:

  Prior scale for item difficulty or location.

- guessing_mean:

  Prior mean for the lower-asymptote or guessing parameter.
