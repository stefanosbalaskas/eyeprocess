# Plan or fit the focused M3-to-M4 ablation set

The default is plan-only. The essential lattice compares M3, formal K=1,
reference K=2, K=2 without trait conditioning, and K=2 with iid states.
Optional RT-anchored channel ablations are available but are not part of
the default development burden. A no-RT K\>1 model is intentionally
excluded because the reference label-identification policy orders RT
state deviations.

## Usage

``` r
multimodal_m4_ablation(
  x,
  run = FALSE,
  include_channel_ablations = FALSE,
  m3_fit = NULL,
  m4_fit = NULL,
  fit_args = list()
)
```

## Arguments

- x:

  Data or M4 simulation.

- run:

  Whether to fit models. Default \`FALSE\`.

- include_channel_ablations:

  Add RT-only, no-gaze, and no-pupil state models.

- m3_fit, m4_fit:

  Optional already fitted baseline/reference objects to reuse.

- fit_args:

  Sampling arguments shared across new fits.

## Value

An \`eye_multimodal_m4_ablation\`.
