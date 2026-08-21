# Construct M4 temporal, nuisance, device, and overfitting negative controls

Controls are transformations/stress designs by default and therefore do
not incur backend fitting. Set \`run = TRUE\` only when explicit fitted
comparisons are needed.

## Usage

``` r
multimodal_m4_negative_controls(
  x,
  controls = c("order_shuffle", "process_shuffle", "state_independent",
    "nuisance_pseudostate", "device_session_pseudostate", "overfit_state_count"),
  seed = 20260820L,
  run = FALSE,
  fit_args = list()
)
```

## Arguments

- x:

  Data or M4 simulation.

- controls:

  Control names.

- seed:

  Deterministic seed.

- run:

  Whether to fit the generated controls.

- fit_args:

  Arguments for optional M4 fits.

## Value

An \`eye_multimodal_m4_negative_controls\`.
