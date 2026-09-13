# Compare explicit process-model specifications

Compare explicit process-model specifications

## Usage

``` r
compare_process_models(
  data,
  methods,
  analysis_fun,
  extract_fun = .ep09_default_sensitivity_extract
)
```

## Arguments

- data:

  Data.

- methods:

  Named methods/specifications.

- analysis_fun:

  Function \`(data, method, specification)\`.

- extract_fun:

  Result extractor.

## Value

An object of class "eye_process_sensitivity", stored as a named list,
with components "grid", "results", "failures", "warnings", "grid_hash",
"created_at", "status", "caveat". It contains explicit process-model
specifications and associated metadata or diagnostics needed to
interpret the result.
