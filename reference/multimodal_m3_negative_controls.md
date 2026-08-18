# Generate M3 multimodal falsification controls

Includes within-item RT/gaze/pupil permutations, within-person pupil
permutation across items, pupil phase randomization, luminance-only
pseudo-pupil, and an irrelevant synthetic channel. Controls preserve
selected marginals while deliberately breaking alignment; they are not
causal interventions or misconduct detectors.

## Usage

``` r
multimodal_m3_negative_controls(x, seed = 20260815L)
```

## Arguments

- x:

  M3-compatible data.

- seed:

  Deterministic seed.

## Value

An \`eye_multimodal_m3_negative_controls\`.
