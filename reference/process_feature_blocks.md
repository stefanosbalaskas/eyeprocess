# Define conceptual process-feature blocks

Define conceptual process-feature blocks

## Usage

``` r
process_feature_blocks(data, blocks, id = NULL, drop_constant = TRUE)
```

## Arguments

- data:

  Data frame.

- blocks:

  Named list of column names for conceptual blocks.

- id:

  Optional identifier column.

- drop_constant:

  Remove non-varying columns.

## Value

An object of class "eye_process_feature_blocks", stored as a named list,
with components "data", "blocks", "id", "block_sizes", "status". It
contains define conceptual process-feature blocks and associated
metadata or diagnostics needed to interpret the result.
