# Audit category probability functions

Audit category probability functions

## Usage

``` r
eyeprocess_irt_category_function_audit(probabilities, tolerance = 1e-08)
```

## Arguments

- probabilities:

  Probability matrix or vector, with dimensions appropriate to the
  model.

- tolerance:

  Numerical or decision tolerance.

## Value

An object of class "eye_irt_category_audit", stored as a named list,
with components "valid_bounds", "rows_sum_to_one", "max_sum_error",
"min_probability", "max_probability". It contains category probability
functions and associated metadata or diagnostics needed to interpret the
result.
