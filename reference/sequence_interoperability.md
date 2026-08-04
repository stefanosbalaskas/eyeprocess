# Convert scanpaths to process/sequence package contracts

Convert scanpaths to process/sequence package contracts

## Usage

``` r
as_procdata_sequence(
  x,
  source = c("visits", "fixations", "samples"),
  collapse_consecutive = TRUE
)

as_traminer_sequence(
  x,
  source = c("visits", "fixations", "samples"),
  collapse_consecutive = TRUE,
  create_object = FALSE
)

as_seqhmm_data(
  x,
  source = c("visits", "fixations", "samples"),
  collapse_consecutive = TRUE
)
```

## Arguments

- x:

  An \`eye_dataset\`.

- source:

  AOI sequence source.

- collapse_consecutive:

  Whether to collapse repeated adjacent states.

- create_object:

  For \`as_traminer_sequence()\`, whether to return a native
  \`TraMineR\` sequence object instead of the package-neutral wide
  table.

## Value

A package-compatible representation.
