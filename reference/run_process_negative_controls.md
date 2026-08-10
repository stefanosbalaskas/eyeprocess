# Run repeated process negative controls

Run repeated process negative controls

## Usage

``` r
run_process_negative_controls(
  data,
  outcome,
  analysis_fun,
  controls = c("permutation", "shift"),
  replications = 100L,
  seed = 1L,
  extract_fun = .ep09_default_control_extract,
  shift_lags = c(-3L, -2L, -1L, 1L, 2L, 3L),
  within = NULL
)
```

## Arguments

- data:

  Data frame.

- outcome:

  Outcome column.

- analysis_fun:

  Function applied to each negative-control dataset.

- controls:

  Character vector among \`permutation\` and \`shift\`.

- replications:

  Number of controls per type.

- seed:

  Seed.

- extract_fun:

  Function converting analysis result to a data.frame.

- shift_lags:

  Lags sampled for shift controls.

- within:

  Optional permutation groups.
