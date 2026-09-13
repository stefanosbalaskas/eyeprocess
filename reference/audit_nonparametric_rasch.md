# Run nonparametric Rasch diagnostics with eRm

Run nonparametric Rasch diagnostics with eRm

## Usage

``` r
audit_nonparametric_rasch(
  response_matrix,
  methods = c("T1", "T10"),
  n = 100L,
  splitcr = "median",
  seed = 321
)
```

## Arguments

- response_matrix:

  Dichotomous response matrix.

- methods:

  eRm NPtest methods, e.g. T1 and T10.

- n:

  Number of sampled matrices.

- splitcr:

  Split criterion for tests that use one.

- seed:

  Seed.

## Value

An object of class "eye_nonparametric_rasch_audit", stored as a named
list, with components "tests", "status", "n", "splitcr", "caveat". It
contains nonparametric Rasch diagnostics with eRm and associated
metadata or diagnostics needed to interpret the result.
