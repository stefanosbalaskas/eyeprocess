# Fit the complete M3 response-anchored channel-ablation lattice

Fits all eight response-anchored combinations of RT, gaze and pupil.
Models use one common subset-likelihood implementation to make the
target and prior family explicit. This is inferential ablation, not a
causal intervention on sensors.

## Usage

``` r
multimodal_m3_ablation(
  x,
  models = NULL,
  nuisance = stats::setNames(rep(TRUE, 8L), .ep10_m3_nuisance_names),
  ...
)
```

## Arguments

- x:

  M3-compatible data.

- models:

  Optional subset of the eight model identifiers.

- nuisance:

  Named logical vector selecting pupil nuisance terms. The same
  selection is applied to every ablation model containing pupil.

- ...:

  Sampling controls forwarded to the subset fitter.

## Value

An \`eye_multimodal_m3_ablation\`.
