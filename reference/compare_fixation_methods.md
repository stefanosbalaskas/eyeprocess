# Compare explicit fixation-detection methods

Compare explicit fixation-detection methods

## Usage

``` r
compare_fixation_methods(
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
