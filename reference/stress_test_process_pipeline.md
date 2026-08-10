# Stress-test an analysis under explicit synthetic corruptions

Stress-test an analysis under explicit synthetic corruptions

## Usage

``` r
stress_test_process_pipeline(
  data,
  plans,
  analysis_fun,
  metric_fun = .ep09_default_sensitivity_extract,
  ...
)
```

## Arguments

- data:

  Baseline data.

- plans:

  List of corruption plans.

- analysis_fun:

  Function \`(data, plan)\`.

- metric_fun:

  Function \`(analysis_result, plan)\` returning scalar/list/data.frame
  metrics.

- ...:

  Passed to \`apply_synthetic_corruption()\`.
