# Validate M4 data, computation, state behavior, and evidence

Combines structural identifiability, sampler diagnostics, state
uncertainty, PPC, and optionally supplied
information/negative-control/sensitivity/recovery evidence into
domain-specific statuses. \`PASS\` means the declared checks pass; it
never means that state labels have substantive psychological validity.

## Usage

``` r
validate_multimodal_m4(
  x,
  information = NULL,
  negative_controls = NULL,
  sensitivity = NULL,
  recovery = NULL,
  include_ppc = TRUE
)
```

## Arguments

- x:

  M4 fit.

- information:

  Optional M4 information object.

- negative_controls:

  Optional M4 negative-controls object.

- sensitivity:

  Optional M4 sensitivity object.

- recovery:

  Optional M4 recovery object.

- include_ppc:

  Whether to compute PPC summaries.

## Value

An \`eye_multimodal_m4_validation\`.
