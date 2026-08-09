# Filter a one-dimensional eye signal robustly

Filter a one-dimensional eye signal robustly

## Usage

``` r
filter_eye_signal(
  signal,
  width = 9L,
  method = c("auto", "robfilter", "runmed"),
  online = TRUE
)
```

## Arguments

- signal:

  Numeric signal.

- width:

  Median-filter width.

- method:

  \`auto\`, \`robfilter\`, or \`runmed\`.

- online:

  Passed to \`robfilter::med.filter()\` when used.

## Value

An \`eye_signal_filter_audit\` object.
