# Specify a model-validation programme

Specify a model-validation programme

## Usage

``` r
model_validation_spec(
  replications = 100L,
  confidence = 0.95,
  max_abs_bias = 0.1,
  min_coverage = 0.9,
  max_failure_rate = 0.05
)
```

## Arguments

- replications:

  Number of Monte Carlo replications.

- confidence:

  Confidence level for interval coverage.

- max_abs_bias:

  Maximum acceptable absolute bias.

- min_coverage:

  Minimum acceptable interval coverage.

- max_failure_rate:

  Maximum acceptable estimation failure rate.

## Value

An \`eye_model_validation_spec\`.
