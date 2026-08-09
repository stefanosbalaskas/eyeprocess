# Fit a process-informed Rasch tree

Fit a process-informed Rasch tree

## Usage

``` r
fit_process_rasch_tree(
  response_matrix,
  covariates,
  formula = NULL,
  maxit = 60L
)
```

## Arguments

- response_matrix:

  Person x item dichotomous response matrix.

- covariates:

  Person-level response-process covariates.

- formula:

  Optional splitting formula. If omitted, all covariates are used.

- maxit:

  Maximum model iterations.
