# Negative-control test for an allegedly informative process channel

The user supplies a complete evaluation callback. eyeprocess permutes
the named process columns, optionally within grouping strata, and
compares the observed score with the permutation distribution.

## Usage

``` r
negative_control_process_test(
  data,
  process_columns,
  evaluator,
  within = NULL,
  permutations = 100L,
  higher_is_better = TRUE,
  seed = 20260808L
)
```

## Arguments

- data:

  Input data.

- process_columns:

  Columns to permute.

- evaluator:

  Function returning one scalar out-of-sample score.

- within:

  Optional grouping columns within which permutation occurs.

- permutations:

  Number of negative-control permutations.

- higher_is_better:

  Score direction.

- seed:

  Random-number seed.
