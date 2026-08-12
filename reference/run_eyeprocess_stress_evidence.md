# Execute a declared measurement-stress evidence plan

\`corruptors\` is a named list of functions accepting \`(data, severity,
seed)\`. \`metric_fun\` must return a named finite/numeric vector (NA is
allowed for metrics that are undefined in a scenario). The executor
records software behavior under declared corruptions and does not define
universal data- quality thresholds.

## Usage

``` r
run_eyeprocess_stress_evidence(data, plan, corruptors, metric_fun)
```

## Arguments

- data:

  Input data frame, matrix, or compatible analysis object.

- plan:

  Validation or stress-evidence plan object.

- corruptors:

  Named list of corruption functions used by the stress programme.

- metric_fun:

  Function used to compute the stress-programme evaluation metric.
