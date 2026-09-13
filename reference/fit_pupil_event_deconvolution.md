# Fit transparent event-related pupil deconvolution models

Fits a linear superposition model of overlapping event kernels. This is
a transparent reference deconvolution layer, not a universal
physiological model.

## Usage

``` r
fit_pupil_event_deconvolution(
  data,
  by = c("person_id", "trial_id"),
  time = "time_ms",
  pupil = "pupil_bc",
  events = list(stimulus = 0),
  tmax_ms = 930,
  shape = 10.1,
  min_samples = 20L
)
```

## Arguments

- data:

  Sample-level pupil data.

- by:

  Grouping columns, typically person and trial.

- time, pupil:

  Column names.

- events:

  Named list mapping event labels to scalar event times or columns.

- tmax_ms, shape:

  Kernel parameters.

- min_samples:

  Minimum usable samples per group.

## Value

An object of class "eye_pupil_deconvolution", stored as a named list,
with components "fits", "effects", "fitted", "events", "tmax_ms",
"shape", "by", "status", "caveat". It contains transparent event-related
pupil deconvolution models and associated metadata or diagnostics needed
to interpret the result.
