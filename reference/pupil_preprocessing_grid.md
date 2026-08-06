# Create a preprocessing sensitivity grid for pupil analysis

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
pupil_preprocessing_grid(baseline_windows = list(c(-200, 0), c(-500, 0)),
  latency_ms = c(100, 200, 300), basis_df = c(4L, 6L, 8L),
  baseline_methods = c("subtract", "percent"), max_interpolated_fraction = c(0.10,
  0.20))
```

## Arguments

- baseline_windows:

  List of baseline windows.

- latency_ms:

  Latency shifts.

- basis_df:

  Basis degrees of freedom.

- baseline_methods:

  Baseline corrections.

- max_interpolated_fraction:

  Interpolation thresholds.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
