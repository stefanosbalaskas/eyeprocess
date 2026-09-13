# Dynamic GPIRT external-engine gate

Dynamic GPIRT external-engine gate

## Usage

``` r
fit_dynamic_gpirt(data, external_engine = NULL, ...)
```

## Arguments

- data:

  Input data frame or compatible tabular object.

- external_engine:

  Validated external fitting function.

- ...:

  Additional arguments passed to the selected model, engine, or method.

## Value

An object of class "eye_dynamic_gpirt", stored as a named list, with
components "model", "engine", "status". It contains dynamic GPIRT
external-engine gate and associated metadata or diagnostics needed to
interpret the result.
