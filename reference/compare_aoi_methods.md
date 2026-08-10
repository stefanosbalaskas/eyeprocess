# Compare explicit AOI assignment methods

Compare explicit AOI assignment methods

## Usage

``` r
compare_aoi_methods(
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
