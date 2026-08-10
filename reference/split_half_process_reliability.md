# Split-half reliability for a trial-level process measure

Split-half reliability for a trial-level process measure

## Usage

``` r
split_half_process_reliability(
  data,
  person,
  trial,
  measure,
  split = c("odd_even", "random"),
  repetitions = 100L,
  seed = 1L,
  aggregate_fun = mean
)
```

## Arguments

- data:

  Long trial-level data.

- person:

  Participant column.

- trial:

  Trial column.

- measure:

  Measure column.

- split:

  Odd/even or repeated random split.

- repetitions:

  Number of random splits.

- seed:

  Seed.

- aggregate_fun:

  Within-half aggregation function.
