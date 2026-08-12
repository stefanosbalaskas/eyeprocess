# Define a validation acceptance rule

Define a validation acceptance rule

## Usage

``` r
validation_acceptance_rule(
  metric,
  direction = c("max", "min", "between", "equals"),
  threshold,
  upper = NULL,
  tolerance = 0
)
```

## Arguments

- metric:

  Metric name or metric column.

- direction:

  Direction vector used to project multidimensional information.

- threshold:

  Decision or diagnostic threshold.

- upper:

  Optional upper threshold for interval-style acceptance rules.

- tolerance:

  Numerical or decision tolerance.
