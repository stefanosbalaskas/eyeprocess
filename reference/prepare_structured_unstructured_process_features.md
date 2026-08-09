# Prepare leakage-safe structured/unstructured process representations

Prepare leakage-safe structured/unstructured process representations

## Usage

``` r
prepare_structured_unstructured_process_features(
  structured,
  unstructured = NULL,
  fold = NULL,
  builder = NULL,
  id = c("person_id", "item_id"),
  ...
)
```

## Arguments

- structured:

  Structured person/item/window features.

- unstructured:

  Optional sequence/sample object.

- fold:

  Fold identifier. If supplied, representation builders are applied
  fold-locally using \`builder\`.

- builder:

  Optional function \`(train_structured, train_unstructured,
  test_structured, test_unstructured, fold_value, ...)\` returning a
  fold result.

- id:

  Optional identifier columns retained in the representation contract.

- ...:

  Passed to \`builder\`.
