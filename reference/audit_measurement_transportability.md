# Summarise measurement transportability across held-out groups

Summarise measurement transportability across held-out groups

## Usage

``` r
audit_measurement_transportability(
  validation,
  metric,
  higher_is_better = TRUE,
  max_range = NULL,
  minimum = NULL,
  maximum = NULL
)
```

## Arguments

- validation:

  Validation results or validation specification.

- metric:

  Metric to calculate or audit.

- higher_is_better:

  Whether larger metric values indicate better performance.

- max_range:

  Maximum allowed range across held-out groups.

- minimum:

  Minimum acceptable value or threshold.

- maximum:

  Maximum acceptable value or threshold.
