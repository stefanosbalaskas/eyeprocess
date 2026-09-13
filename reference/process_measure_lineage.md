# Process-measure lineage table

Process-measure lineage table

## Usage

``` r
process_measure_lineage(
  measure,
  inputs,
  transformations = character(),
  output_level = NA_character_
)
```

## Arguments

- measure:

  Measure name.

- inputs:

  Required input variable names.

- transformations:

  Ordered transformation labels.

- output_level:

  Output aggregation level.

## Value

An object of class "eye_process_measure_lineage", stored as a named
list, with components "measure", "inputs", "transformations",
"output_level", "lineage_hash". It contains process-measure lineage
table and associated metadata or diagnostics needed to interpret the
result.
