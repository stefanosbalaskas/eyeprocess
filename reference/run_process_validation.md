# Run an empirical process-validation programme

Run an empirical process-validation programme

## Usage

``` r
run_process_validation(
  design,
  simulate_fun = simulate_process_validation_data,
  fit_fun = .ep09_default_validation_fit,
  extract_fun = .ep09_default_validation_extract,
  max_conditions = Inf,
  progress = interactive()
)
```

## Arguments

- design:

  Validation design or expanded condition table.

- simulate_fun:

  Function \`(condition, replication, seed)\` returning a simulation
  object.

- fit_fun:

  Function \`(simulated, condition)\` returning a fitted object.

- extract_fun:

  Function \`(fit, simulated, condition)\` returning one or more rows
  with estimates.

- max_conditions:

  Optional cap on conditions actually run.

- progress:

  Print compact progress messages.

## Value

An \`eye_process_validation_result\` object.
