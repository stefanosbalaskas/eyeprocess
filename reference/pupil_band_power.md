# Compute pupil signal power in a frequency band

Compute pupil signal power in a frequency band

## Usage

``` r
pupil_band_power(y, sampling_rate_hz, lower_hz, upper_hz, detrend = TRUE)
```

## Arguments

- y:

  Pupil signal.

- sampling_rate_hz:

  Sampling rate in Hz.

- lower_hz, upper_hz:

  Frequency-band limits.

- detrend:

  Remove the mean before FFT.
