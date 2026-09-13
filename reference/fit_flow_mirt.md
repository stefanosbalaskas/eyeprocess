# Flow-MIRT external-engine gate

Flow-MIRT external-engine gate

## Usage

``` r
fit_flow_mirt(response_matrix, external_engine = NULL, ...)
```

## Arguments

- response_matrix:

  Person-by-item response matrix.

- external_engine:

  Validated external fitting function.

- ...:

  Additional arguments passed to the selected model, engine, or method.

## Value

An object of class "eye_flow_mirt", stored as a named list, with
components "model", "status", "engine". It contains flow-MIRT
external-engine gate and associated metadata or diagnostics needed to
interpret the result.
