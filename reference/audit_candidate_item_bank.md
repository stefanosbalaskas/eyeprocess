# Audit a candidate item bank against a seed model

Audit a candidate item bank against a seed model

## Usage

``` r
audit_candidate_item_bank(
  object,
  candidate_data,
  difficulty_range = c(-3, 3),
  discrimination_min = 0.3
)
```

## Arguments

- object:

  Seed model.

- candidate_data:

  Candidate item feature data.

- difficulty_range:

  Plausible screening range for predicted difficulty.

- discrimination_min:

  Minimum screening discrimination.

## Value

An object of class "eye_candidate_item_bank_audit", stored as a named
list, with components "table", "seed_model", "status", "caveat". It
contains a candidate item bank against a seed model and associated
metadata or diagnostics needed to interpret the result.
