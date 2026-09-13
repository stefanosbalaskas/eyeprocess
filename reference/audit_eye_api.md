# Audit API lifecycle completeness and replacement contracts

Audit API lifecycle completeness and replacement contracts

## Usage

``` r
audit_eye_api(inventory = eye_api_inventory(), registry = eye_api_lifecycle())
```

## Arguments

- inventory:

  API inventory.

- registry:

  Lifecycle registry.

## Value

An object of class "eye_api_audit", stored as a named list, with
components "table", "unreviewed", "invalid_replacements",
"invalid_canonical", "reviewed_fraction", "valid". It contains aPI
lifecycle completeness and replacement contracts and associated metadata
or diagnostics needed to interpret the result.
