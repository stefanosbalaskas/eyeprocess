# Combine invariance evidence without converting it to a binary validity claim

Combine invariance evidence without converting it to a binary validity
claim

## Usage

``` r
eyeprocess_irt_invariance_evidence(
  anchor_audit = NULL,
  dif = NULL,
  dtf = NULL,
  linking = NULL,
  process_concordance = NULL
)
```

## Arguments

- anchor_audit:

  Anchor-item audit result.

- dif:

  Differential-item-functioning evidence or summary.

- dtf:

  Differential-test-functioning evidence or curve.

- linking:

  Scale-linking evidence or result.

- process_concordance:

  Process-DIF concordance evidence.

## Value

An object of class "eye_irt_invariance_evidence", stored as a named
list, with components "components", "present", "completeness",
"interpretation". It contains combine invariance evidence without
converting it to a binary validity claim and associated metadata or
diagnostics needed to interpret the result.
