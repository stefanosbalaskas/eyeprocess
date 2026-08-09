# Plot tonic/phasic pupil components

Plot tonic/phasic pupil components

## Usage

``` r
plot_pupil_components(
  data,
  time = "time_ms",
  smoothed = "pupil_smoothed",
  tonic = "pupil_tonic",
  phasic = "pupil_phasic",
  ...
)
```

## Arguments

- data:

  Data frame containing the required process variables.

- time:

  Time values or name of the time variable.

- smoothed:

  Name of the smoothed pupil-signal column.

- tonic:

  Name of the tonic pupil-component column.

- phasic:

  Name of the phasic pupil-component column.

- ...:

  Additional arguments passed to the underlying method or helper.
