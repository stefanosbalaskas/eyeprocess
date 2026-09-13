# Build a provenance lineage node table

Build a provenance lineage node table

## Usage

``` r
provenance_lineage_table(
  id,
  type = "entity",
  label = id,
  value = NA_character_
)
```

## Arguments

- id:

  Node identifiers.

- type:

  Node types such as entity, activity, agent.

- label:

  Human-readable labels.

- value:

  Optional values/locations.

## Value

A data frame containing a provenance lineage node table. Rows represent
the analysis units and columns contain the identifiers, estimates, or
diagnostics defined by the function.
