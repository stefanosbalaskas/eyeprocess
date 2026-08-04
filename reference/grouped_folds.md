# Create grouped cross-validation folds

Create grouped cross-validation folds

## Usage

``` r
grouped_folds(data, group = c("participant_id"), v = 5L, seed = 1L)
```

## Arguments

- data:

  Data frame.

- group:

  Grouping columns that must not cross folds.

- v:

  Number of folds.

- seed:

  Random seed.

## Value

An \`eye_grouped_folds\` object.
