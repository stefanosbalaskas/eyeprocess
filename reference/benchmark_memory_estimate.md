# Memory estimate for an R object or generated problem size

Memory estimate for an R object or generated problem size

## Usage

``` r
benchmark_memory_estimate(x, generator = NULL)
```

## Arguments

- x:

  Object, or numeric n when \`generator\` is supplied.

- generator:

  Optional function taking n.

## Value

A data frame containing memory estimate for an R object or generated
problem size. Rows represent the analysis units and columns contain the
identifiers, estimates, or diagnostics defined by the function.
