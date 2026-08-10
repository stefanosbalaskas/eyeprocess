# Export lightweight PROV-oriented JSON

This is a compact interoperability representation inspired by W3C PROV.
It is not asserted to be a complete PROV-O serialization; consumers
requiring full conformance should validate/transform it externally.

## Usage

``` r
export_prov_json(x, path)
```

## Arguments

- x:

  Provenance graph.

- path:

  Output JSON path.
