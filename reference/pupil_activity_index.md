# Compute a transparent pupil activity index

Compute a transparent pupil activity index

## Usage

``` r
pupil_activity_index(
  y,
  time_ms = seq_along(y),
  sampling_rate_hz = NULL,
  method = c("velocity", "frequency_contrast", "ripa_proxy"),
  low_band = c(0.05, 0.5),
  high_band = c(0.5, 4),
  fast_window_ms = 250,
  slow_window_ms = 750
)
```

## Arguments

- y:

  Pupil signal.

- time_ms:

  Time vector.

- sampling_rate_hz:

  Sampling rate for frequency methods.

- method:

  \`velocity\`, \`frequency_contrast\`, or \`ripa_proxy\`.

- low_band, high_band:

  Frequency bands for frequency contrast.

- fast_window_ms, slow_window_ms:

  Smoothing windows for the RIPA-style proxy.
