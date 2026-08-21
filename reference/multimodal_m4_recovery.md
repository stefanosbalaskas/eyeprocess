# Evaluate deterministic M4 parameter and state recovery

Recovery aligns latent-state labels before scoring multichannel state
effects, and emphasizes posterior-probability calibration, occupancy,
and transition recovery rather than treating MAP classification accuracy
as the primary criterion. By default the function returns a small
five-scenario design and does not launch expensive fitting.

## Usage

``` r
multimodal_m4_recovery(
  simulation = NULL,
  fit = NULL,
  scenarios = c("clear", "weak", "null", "trait_conditioned", "nuisance_confounded"),
  run = FALSE,
  simulation_args = list(n_person = 60L, n_item = 10L),
  fit_args = list()
)
```

## Arguments

- simulation:

  Optional single M4 simulation.

- fit:

  Optional already-fitted M4 model corresponding to \`simulation\`.

- scenarios:

  Deterministic development battery used when \`run = TRUE\`.

- run:

  Whether to execute the small recovery battery. Default \`FALSE\`.

- simulation_args:

  Named arguments forwarded to simulation.

- fit_args:

  Named arguments forwarded to \`fit_multimodal_m4()\`.

## Value

An \`eye_multimodal_m4_recovery\`.
