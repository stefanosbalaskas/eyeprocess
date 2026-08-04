# Compare equivalent model engines

Compare equivalent model engines

## Usage

``` r
compare_model_engines(
  data,
  engines,
  extractors,
  reference = names(engines)[1L],
  tolerance = 0.05
)
```

## Arguments

- data:

  Model-ready data.

- engines:

  Named list of fitting functions receiving \`data\`.

- extractors:

  Named list of extractor functions or one shared extractor.

- reference:

  Optional reference engine name.

- tolerance:

  Maximum absolute estimate difference for equivalence.

## Value

An \`eye_engine_comparison\` object.
