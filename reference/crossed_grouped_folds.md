# Create cross-classified grouped folds

Holds out levels from every declared grouping dimension simultaneously.
The assessment set is the intersection of held-out levels; the analysis
set excludes every held-out level. Rows combining held-out and retained
levels form a buffer and are deliberately used in neither set.

## Usage

``` r
crossed_grouped_folds(
  data,
  groups = c("participant_id", "item_id"),
  v = 5L,
  seed = 1L
)
```

## Arguments

- data:

  Data frame.

- groups:

  Two or more crossed grouping columns.

- v:

  Number of folds.

- seed:

  Random seed.

## Value

An \`eye_crossed_grouped_folds\` object.
