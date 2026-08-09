# Apply a pre-flight decision to data explicitly

Apply a pre-flight decision to data explicitly

## Usage

``` r
apply_preflight_decision(
  data,
  audit,
  keep_decisions = c("pass_preflight", "use_with_caution")
)
```

## Arguments

- data:

  Original data.

- audit:

  Pre-flight audit.

- keep_decisions:

  Decisions to retain.

## Value

Filtered data with an attached \`preflight_application\` attribute.
