# Cross-classified process IRT reference model

Treats the process outcome as repeated evidence crossed by person and
item, with optional contextual grouping factors. This is useful when
process events themselves, not only item summaries, are the
observations.

## Usage

``` r
fit_crossclassified_process_irt(
  data,
  outcome,
  person = "participant_id",
  item = "item_id",
  context = NULL,
  family = c("gaussian", "binomial", "poisson", "negative_binomial"),
  fixed = NULL
)
```

## Arguments

- data:

  Input data frame or compatible tabular object.

- outcome:

  Outcome variable.

- person:

  Person or participant identifier column.

- item:

  Item identifier, name, or item column.

- context:

  Context or grouping variable.

- family:

  Statistical family used by the channel or model.

- fixed:

  Fixed-effects specification.

## Value

An object of class "eye_crossclassified_process_irt", stored as a named
list, with components "model", "family", "person", "item", "context",
"status". It contains cross-classified process IRT reference model and
associated metadata or diagnostics needed to interpret the result.
