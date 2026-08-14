# Audit structural and data identifiability for M0-M2

This is a conservative pre-fit audit. It does not prove global
identifiability. It verifies support, design connectivity, channel
coverage, response variation, and the explicit scale constraints used by
the reference likelihoods.

## Usage

``` r
audit_multimodal_m2_identifiability(
  x,
  person = "person_id",
  item = "item_id",
  response = "response",
  rt = "rt",
  gaze = "gaze",
  model = c("M2", "M1", "M0"),
  min_persons = 20L,
  min_items = 5L
)
```

## Arguments

- x:

  Data frame or compatible M2 object.

- person, item, response, rt, gaze:

  Column names.

- model:

  \`"M0"\`, \`"M1"\`, or \`"M2"\`.

- min_persons, min_items:

  Conservative design thresholds.

## Value

An \`eye_multimodal_m2_identifiability\` object.
