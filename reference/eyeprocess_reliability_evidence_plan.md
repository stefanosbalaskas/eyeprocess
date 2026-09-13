# Declare reliability evidence targets

Declare reliability evidence targets

## Usage

``` r
eyeprocess_reliability_evidence_plan(
  metrics = c("split_half", "icc", "temporal_stability", "bland_altman"),
  bootstrap = 200L,
  seed = 20260811L
)
```

## Arguments

- metrics:

  Reliability metrics requested by the evidence plan.

- bootstrap:

  Number of bootstrap replicates or bootstrap configuration.

- seed:

  Random-number seed for reproducible execution.

## Value

An object of class "eye_reliability_evidence_plan", stored as a named
list, with components "metrics", "bootstrap", "seed", "guardrail". It
contains declare reliability evidence targets and associated metadata or
diagnostics needed to interpret the result.
