# Simulate data from a model or registered model specification

Simulate data from a model or registered model specification

## Usage

``` r
simulate_from_model(model, ...)
```

## Arguments

- model:

  Registered model id/specification, simulation function, or an object
  exposing a \`simulate_fun\` function.

- ...:

  Arguments passed to the simulator.

## Value

An R object containing data from a model or registered model
specification. The concrete class and structure follow the selected
method, engine, or input object and are preserved as documented by that
workflow.
