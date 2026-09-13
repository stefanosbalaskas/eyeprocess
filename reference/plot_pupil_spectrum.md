# Plot a pupil-signal power spectrum

Plot a pupil-signal power spectrum

## Usage

``` r
plot_pupil_spectrum(signal, sampling_rate_hz, max_hz = sampling_rate_hz/2, ...)
```

## Arguments

- signal:

  Numeric pupil signal.

- sampling_rate_hz:

  Sampling rate.

- max_hz:

  Maximum frequency shown; defaults to Nyquist.

- ...:

  Additional arguments passed to the underlying method or helper.

## Value

A data frame containing plot a pupil-signal power spectrum. Rows
represent the analysis units and columns contain the identifiers,
estimates, or diagnostics defined by the function.
