# M3 response + RT + gaze + pupil specification

Extends the established eyeprocess multimodal IRT specification with a
continuous pupil-responsivity channel. The summary representation is
fitted by the bundled four-channel Stan model. \`functional_score\`
records that the pupil input was derived from a trajectory/functional
workflow; it does not silently turn the scalar reference likelihood into
a functional likelihood.

## Usage

``` r
multimodal_m3_spec(
  backend = "cmdstanr",
  prior_profile = c("regularized", "paper_centered"),
  missingness = "ignorable",
  pupil_representation = c("summary", "functional_score"),
  nuisance = stats::setNames(rep(TRUE, 8L), .ep10_m3_nuisance_names)
)
```

## Arguments

- backend:

  Currently \`"cmdstanr"\` only.

- prior_profile:

  Prior profile.

- missingness:

  Currently \`"ignorable"\` only.

- pupil_representation:

  \`"summary"\` or \`"functional_score"\`.

- nuisance:

  Named logical vector selecting baseline, luminance, gaze X/Y, quality,
  blink, interpolation and time-on-task nuisance terms. Availability is
  also audited from data.

## Value

An \`eye_multimodal_m3_spec\` inheriting the established multimodal and
IRT spec classes.
