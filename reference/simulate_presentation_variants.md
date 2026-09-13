# Simulate pre-registered presentation variants for review

Simulate pre-registered presentation variants for review

## Usage

``` r
simulate_presentation_variants(
  audit,
  line_spacing_multiplier = 1.25,
  key_term_highlighting = TRUE
)
```

## Arguments

- audit:

  An accessibility audit.

- line_spacing_multiplier:

  Example line-spacing multiplier for flagged rows.

- key_term_highlighting:

  Whether the simulated review variant highlights key terms.

## Value

An R object containing pre-registered presentation variants for review.
The concrete class and structure follow the selected method, engine, or
input object and are preserved as documented by that workflow.
