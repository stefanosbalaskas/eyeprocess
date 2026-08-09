# Plot raw-to-processed pupil preprocessing stages

Plot raw-to-processed pupil preprocessing stages

## Usage

``` r
plot_pupil_preprocessing_audit(
  data,
  time = "time_ms",
  signals = c("pupil_raw", "pupil_interpolated", "pupil_smoothed", "pupil_bc"),
  ...
)
```

## Arguments

- data:

  Sample-level data.

- time:

  Time column.

- signals:

  Signal columns to overlay.

- ...:

  Additional arguments passed to the underlying method or helper.
