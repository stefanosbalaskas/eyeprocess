# Compare an observed effect against a negative-control null distribution

Compare an observed effect against a negative-control null distribution

## Usage

``` r
process_null_benchmark(observed, controls, effect = "effect")
```

## Arguments

- observed:

  Observed scalar effect.

- controls:

  Negative-control result or numeric vector.

- effect:

  Effect column when controls is an object.

## Value

A named list with components "observed", "n_null", "null_mean",
"null_sd", "percentile", "two_sided_tail", "standardized_distance",
containing an observed effect against a negative-control null
distribution and associated metadata or diagnostics.
