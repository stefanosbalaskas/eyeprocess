# Declare a vendor semantic schema contract

Declare a vendor semantic schema contract

## Usage

``` r
vendor_schema_contract(
  vendor,
  version = NA_character_,
  required_fields = character(),
  optional_fields = character(),
  aliases = list(),
  timestamp = list(),
  coordinate = list(),
  units = list(),
  eye_streams = character(),
  event_fields = character()
)
```

## Arguments

- vendor:

  Vendor/ecosystem label.

- version:

  Optional format/software version.

- required_fields:

  Fields that must survive import.

- optional_fields:

  Fields that may be present.

- aliases:

  Named list mapping canonical fields to accepted vendor names.

- timestamp:

  Named list describing device/system/media time columns.

- coordinate:

  Named list describing x/y columns and coordinate semantics.

- units:

  Named list of expected units for canonical fields.

- eye_streams:

  Expected eye streams (\`left\`, \`right\`, \`cyclopean\`, etc.).

- event_fields:

  Event/annotation fields expected to survive.
