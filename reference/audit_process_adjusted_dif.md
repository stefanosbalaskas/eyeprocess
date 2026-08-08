# Audit DIF before and after process-data adjustment

Audit DIF before and after process-data adjustment

## Usage

``` r
audit_process_adjusted_dif(
  data,
  response = "response",
  ability,
  group,
  item = "item_id",
  process_features,
  person = "participant_id"
)
```

## Arguments

- data:

  Input data frame or compatible tabular object.

- response:

  Response variable or response-column name.

- ability:

  Value supplied to \`ability\`; see Details for its model-specific
  role.

- group:

  Value supplied to \`group\`; see Details for its model-specific role.

- item:

  Item identifier, name, or item column.

- process_features:

  Names of process-derived features.

- person:

  Person or participant identifier column.
