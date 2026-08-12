# Select the most informative eligible item at a theta estimate

Select the most informative eligible item at a theta estimate

## Usage

``` r
eyeprocess_irt_item_selection(
  bank,
  theta,
  administered = character(),
  exposure = NULL,
  content_required = NULL,
  D = 1
)
```

## Arguments

- bank:

  Validated item-bank object.

- theta:

  Latent-trait value or vector of latent-trait values.

- administered:

  Identifiers or records for administered items.

- exposure:

  Item exposure information used by the adaptive-selection rule.

- content_required:

  Content constraints required for item selection.

- D:

  Logistic scaling constant.
