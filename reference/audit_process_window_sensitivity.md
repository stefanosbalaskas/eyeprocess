# Audit sensitivity of process summaries to temporal window choices

Audit sensitivity of process summaries to temporal window choices

## Usage

``` r
audit_process_window_sensitivity(
  data,
  widths_ms = c(250, 500, 1000, 1500),
  steps_ms = c(100, 250, 500),
  metric = "pupil_mean",
  grid = TRUE,
  ...
)
```

## Arguments

- data:

  Sample-level data.

- widths_ms:

  Window widths.

- steps_ms:

  Step widths; recycled or crossed depending on \`grid\`.

- metric:

  Extracted process metric to compare.

- grid:

  If TRUE, evaluate all width-step combinations.

- ...:

  Passed to \`extract_process_windows()\`.
