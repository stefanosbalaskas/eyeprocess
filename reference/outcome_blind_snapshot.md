# Create an outcome-blind data snapshot

Create an outcome-blind data snapshot

## Usage

``` r
outcome_blind_snapshot(data, outcome, id = NULL)
```

## Arguments

- data:

  Data frame.

- outcome:

  Outcome column(s) to remove from the analysis snapshot.

- id:

  Optional identifier columns retained in the snapshot.

## Value

An object of class "eye_outcome_blind_snapshot", stored as a named list,
with components "data", "removed_outcomes", "id", "source_columns",
"blinded_columns", "blinded_hash", "created_at", "caveat". It contains
an outcome-blind data snapshot and associated metadata or diagnostics
needed to interpret the result.
