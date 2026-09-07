# Build a measurement-to-generalization validation ladder

Build a measurement-to-generalization validation ladder

## Usage

``` r
validation_ladder(
  acquisition_qc = "not_assessed",
  analytical_qc = "not_assessed",
  construct_check = "not_assessed",
  within_person = "not_assessed",
  held_out_person = "not_assessed",
  claim = "descriptive"
)
```

## Arguments

- acquisition_qc, analytical_qc, construct_check, within_person,
  held_out_person:

  Stage statuses: \`pass\`, \`warning\`, \`fail\`, or \`not_assessed\`.

- claim:

  Claim type; use \`generalizable\` for out-of-person claims.

## Value

A structured validation-ladder result.
