# Quantify M3 process information, pupil increment, redundancy and sensor value

Uses response-target PSIS-LOO and posterior ability variance across the
eight-channel ablation lattice. It additionally reports paired pupil
gains, a non-additivity/redundancy contrast, a channel-conflict screen,
and optional information per usable pupil observation or sensor cost.
Positive values do not establish construct validity or causal sensor
value.

## Usage

``` r
multimodal_m3_process_information(x, pupil_cost = 1, decisive_z = 2)
```

## Arguments

- x:

  An M3 ablation object.

- pupil_cost:

  Optional positive relative pupil-sensor cost.

- decisive_z:

  Absolute delta/SE ratio used only as a descriptive evidence flag.

## Value

An \`eye_multimodal_m3_information\` object.
