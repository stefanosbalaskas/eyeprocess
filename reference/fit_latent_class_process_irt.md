# Latent process-class IRT reference model

Latent process-class IRT reference model

## Usage

``` r
fit_latent_class_process_irt(
  data,
  response = "response",
  process_features,
  person = "participant_id",
  item = "item_id",
  n_classes = 2L,
  seed = 1
)
```

## Arguments

- data:

  Input data frame or compatible tabular object.

- response:

  Response variable or response-column name.

- process_features:

  Names of process-derived features.

- person:

  Person or participant identifier column.

- item:

  Item identifier, name, or item column.

- n_classes:

  Number of latent classes.

- seed:

  Random-number seed.
