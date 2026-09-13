# Validate imported data against a vendor semantic contract

Validate imported data against a vendor semantic contract

## Usage

``` r
validate_vendor_semantics(data, contract, metadata = list())
```

## Arguments

- data:

  Imported/canonical table.

- contract:

  \`eye_vendor_schema_contract\`.

- metadata:

  Optional named metadata list.

## Value

An object of class "eye_vendor_semantic_validation", stored as a named
list, with components "pass", "vendor", "version", "fields", "aliases",
"timestamp", "units", "contract". It contains imported data against a
vendor semantic contract and associated metadata or diagnostics needed
to interpret the result.
