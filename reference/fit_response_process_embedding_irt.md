# Fit an IRT response model augmented by sequence embeddings

Fit an IRT response model augmented by sequence embeddings

## Usage

``` r
fit_response_process_embedding_irt(
  data,
  sequences,
  response = "response",
  person = "participant_id",
  item = "item_id",
  dimensions = 5L,
  n = c(1L, 2L, 3L)
)
```

## Arguments

- data:

  Input data frame or compatible tabular object.

- sequences:

  Sequence inputs.

- response:

  Response variable or response-column name.

- person:

  Person or participant identifier column.

- item:

  Item identifier, name, or item column.

- dimensions:

  Number of embedding dimensions.

- n:

  Requested count or n-gram order, depending on context.

## Value

An object of class "eye_response_process_embedding_irt", stored as a
named list, with components "model", "embedding", "data", "status". It
contains an IRT response model augmented by sequence embeddings and
associated metadata or diagnostics needed to interpret the result.
