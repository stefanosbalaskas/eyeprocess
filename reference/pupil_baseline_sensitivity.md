# Evaluate pupil baseline-window sensitivity

Evaluate pupil baseline-window sensitivity

## Usage

``` r
pupil_baseline_sensitivity(
  data,
  time = "time_ms",
  pupil = "pupil",
  windows,
  by = NULL,
  correction = c("subtractive", "divisive")
)
```

## Arguments

- data:

  Pupil data.

- time:

  Time column.

- pupil:

  Pupil column.

- windows:

  Named list of two-element baseline windows.

- by:

  Optional grouping columns.

- correction:

  \`subtractive\` or \`divisive\`.
