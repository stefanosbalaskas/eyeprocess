# Predict expected bounded response from a censored-normal process IRT fit

Predict expected bounded response from a censored-normal process IRT fit

## Usage

``` r
# S3 method for class 'eye_censored_normal_process_irt'
predict(object, theta = object$theta, items = NULL, ...)
```

## Arguments

- object:

  A fitted eyeprocess model or audit object.

- theta:

  Latent-trait values.

- items:

  Items to include.

- ...:

  Additional arguments passed to the selected model, engine, or method.
