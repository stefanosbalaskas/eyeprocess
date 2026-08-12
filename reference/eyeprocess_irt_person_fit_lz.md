# Standardized log-likelihood person-fit diagnostic

Computes a Bernoulli response-pattern log-likelihood standardized
against its model-implied mean and variance. Extreme values are model
diagnostics, not evidence of cheating, disengagement, or a psychological
state.

## Usage

``` r
eyeprocess_irt_person_fit_lz(observed, expected, min_probability = 1e-08)
```

## Arguments

- observed:

  Observed responses or observed values.

- expected:

  Model-expected probabilities or expected values.

- min_probability:

  Lower probability bound used for numerical stabilization.
