# Create or normalize a software-paper claim matrix

Create or normalize a software-paper claim matrix

## Usage

``` r
software_paper_claim_matrix(
  claim,
  evidence_id = NA_character_,
  evidence_type = NA_character_,
  status = "pending",
  scope = NA_character_,
  source = NA_character_
)
```

## Arguments

- claim:

  Claim text.

- evidence_id:

  Evidence identifiers.

- evidence_type:

  Evidence type.

- status:

  Status such as supported, qualified, pending, or unsupported.

- scope:

  Explicit scope/qualification.

- source:

  Optional source/location.
