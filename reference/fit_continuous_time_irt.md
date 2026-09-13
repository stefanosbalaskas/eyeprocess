# Continuous-time IRT external-engine gate

Continuous-time IRT external-engine gate

## Usage

``` r
fit_continuous_time_irt(data, external_engine = NULL, ...)
```

## Arguments

- data:

  Input data frame or compatible tabular object.

- external_engine:

  Validated external fitting function.

- ...:

  Additional arguments passed to the selected model, engine, or method.

## Value

An object of class "eye_continuous_time_irt", stored as a named list,
with components "model", "status". It contains continuous-time IRT
external-engine gate and associated metadata or diagnostics needed to
interpret the result.
