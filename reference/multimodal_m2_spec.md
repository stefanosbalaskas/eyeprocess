# M2 response + RT + gaze reference specification

Creates a thin M2 specialization of the established
\[multimodal_irt_spec()\] / \`irt_model_spec()\` architecture. The
measurement likelihood follows Man, Harring, and Zhan (2022): Rasch
response, lognormal response time, and negative-binomial gaze-fixation
counts. Priors are implemented in Stan using either a regularized
profile or a paper-centered profile; the latter is not claimed to
reproduce every published hyperprior exactly.

## Usage

``` r
multimodal_m2_spec(
  backend = "cmdstanr",
  prior_profile = c("regularized", "paper_centered"),
  missingness = "ignorable"
)
```

## Arguments

- backend:

  Currently \`"cmdstanr"\` only.

- prior_profile:

  \`"regularized"\` or \`"paper_centered"\`.

- missingness:

  Currently \`"ignorable"\` only. Channel-specific missing observations
  contribute no level-1 likelihood term.

## Value

An \`eye_multimodal_m2_spec\`, inheriting the established
\`eye_multimodal_irt_spec\` and \`eye_irt_model_spec\` classes.
