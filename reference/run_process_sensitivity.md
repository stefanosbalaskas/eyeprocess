# Run an explicit process-analysis multiverse

Run an explicit process-analysis multiverse

## Usage

``` r
run_process_sensitivity(
  data,
  grid,
  analysis_fun,
  extract_fun = .ep09_default_sensitivity_extract,
  progress = interactive()
)
```

## Arguments

- data:

  Analysis data.

- grid:

  Sensitivity grid.

- analysis_fun:

  Function \`(data, specification)\`.

- extract_fun:

  Function \`(fit, specification)\` returning one or more rows.

- progress:

  Print progress.
