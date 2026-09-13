# Compare explicit pupil-preprocessing methods

Compare explicit pupil-preprocessing methods

## Usage

``` r
compare_pupil_preprocessing(
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
"created_at", "status", "caveat". It contains explicit
pupil-preprocessing methods and associated metadata or diagnostics
needed to interpret the result.
