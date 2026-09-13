# Draw plausible values from a discrete posterior grid

Draw plausible values from a discrete posterior grid

## Usage

``` r
eyeprocess_irt_plausible_values(score, n = 5L, seed = 1L)
```

## Arguments

- score:

  Score object containing posterior or uncertainty information.

- n:

  Number of values, draws, or plausible values to generate.

- seed:

  Random-number seed for reproducible execution.

## Value

An R object containing plausible values from a discrete posterior grid.
The concrete class and structure follow the selected method, engine, or
input object and are preserved as documented by that workflow.
