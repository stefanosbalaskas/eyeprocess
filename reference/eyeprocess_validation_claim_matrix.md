# Build a machine-readable validation claim/evidence matrix

Build a machine-readable validation claim/evidence matrix

## Usage

``` r
eyeprocess_validation_claim_matrix(
  claim_id,
  claim,
  evidence_id,
  evidence_type,
  status = "qualified",
  boundary = NA_character_
)
```

## Arguments

- claim_id:

  Unique identifier for the claim.

- claim:

  Text of the software-validation claim.

- evidence_id:

  Identifier of evidence supporting or testing the claim.

- evidence_type:

  Type or class of evidence.

- status:

  Evidence, model, or governance status.

- boundary:

  Explicit interpretation or scope boundary for the claim.
