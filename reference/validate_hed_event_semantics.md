# Minimal HED annotation audit for event tables

This function checks presence, non-empty annotations and balanced
grouping. It is deliberately a structural audit rather than a full HED
validator; use the official HED validation tooling when formal schema
validation is needed.

## Usage

``` r
validate_hed_event_semantics(events, hed_column = "HED")
```

## Arguments

- events:

  Value supplied to \`events\`; see Details for its model-specific role.

- hed_column:

  Column containing HED annotations.
