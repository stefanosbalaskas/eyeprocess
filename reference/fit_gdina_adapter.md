# Fit a GDINA cognitive-diagnosis adapter

Fit a GDINA cognitive-diagnosis adapter

## Usage

``` r
fit_gdina_adapter(x, q_matrix, model = "GDINA", ...)
```

## Arguments

- x:

  An \`eye_dataset\`.

- q_matrix:

  Q-matrix with items in response-matrix order.

- model:

  GDINA model specification.

- ...:

  Passed to \`GDINA::GDINA()\`.

## Value

An \`eyeprocess_model\`.
