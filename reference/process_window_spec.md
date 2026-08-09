# Specify temporal process windows

Specify temporal process windows

## Usage

``` r
process_window_spec(
  width_ms = 1000,
  step_ms = 500,
  start_ms = 0,
  end_ms = 3000,
  align = c("stimulus", "response", "custom"),
  min_samples = 5L
)
```

## Arguments

- width_ms:

  Window width in milliseconds.

- step_ms:

  Step between successive windows.

- start_ms, end_ms:

  Analysis range relative to the chosen alignment origin.

- align:

  Alignment label, e.g. stimulus, response, or custom.

- min_samples:

  Minimum samples required within a window.

## Value

An \`eye_process_window_spec\` object.
