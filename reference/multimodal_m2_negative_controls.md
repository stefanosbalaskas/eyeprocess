# Generate M2 alignment negative controls

Generates deterministic within-item permutations that preserve each
item's marginal channel distribution while breaking person-level
alignment for gaze, RT, or response. These are falsification controls,
not causal interventions and not misconduct detectors.

## Usage

``` r
multimodal_m2_negative_controls(x, seed = 20260814L)
```

## Arguments

- x:

  M2-compatible data.

- seed:

  Seed for deterministic permutations.

## Value

An \`eye_multimodal_m2_negative_controls\`.
