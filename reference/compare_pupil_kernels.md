# Compare pupil deconvolution kernels

Compare pupil deconvolution kernels

## Usage

``` r
compare_pupil_kernels(data, tmax_values = c(512, 930), ...)
```

## Arguments

- data:

  Same input used for fitting.

- tmax_values:

  Candidate peak times.

- ...:

  Passed to \`fit_pupil_event_deconvolution()\`.

## Value

A tabular R object containing pupil deconvolution kernels; rows
represent analysis units and columns contain the returned quantities.
