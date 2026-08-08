# Conditional censored-normal calibration for bounded process measurements

Fits the 2026 censored-normal response form item-by-item conditional on
a supplied latent score. This is a useful calibration/diagnostic engine
for bounded continuous process variables such as AOI proportions. It is
NOT the paper's full marginal EM estimator and should therefore remain
experimental.

## Usage

``` r
fit_censored_normal_process_irt(
  response_matrix,
  theta,
  lower = 0,
  upper = 1,
  control = list(maxit = 1000)
)
```

## Arguments

- response_matrix:

  Person x item bounded continuous matrix.

- theta:

  Supplied person latent scores on the calibration scale.

- lower, upper:

  Observable bounds.

- control:

  \`optim()\` control list.
