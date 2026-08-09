# Canonical gamma-shaped pupil response kernel

Canonical gamma-shaped pupil response kernel

## Usage

``` r
pupil_response_kernel(
  time_since_event_ms,
  tmax_ms = 930,
  shape = 10.1,
  normalize = TRUE
)
```

## Arguments

- time_since_event_ms:

  Time relative to event onset.

- tmax_ms:

  Approximate response peak time.

- shape:

  Shape parameter.

- normalize:

  Normalize peak to one.
