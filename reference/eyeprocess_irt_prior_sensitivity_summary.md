# Summarise sensitivity of an estimand across declared prior specifications

Summarise sensitivity of an estimand across declared prior
specifications

## Usage

``` r
eyeprocess_irt_prior_sensitivity_summary(
  results,
  prior_id = "prior_id",
  estimate = "estimate"
)
```

## Arguments

- results:

  Results table or analysis results.

- prior_id:

  Identifier for the prior specification.

- estimate:

  Estimate column or numerical estimates to summarize.

## Value

An object of class "eye_irt_prior_sensitivity", stored as a named list,
with components "n_specifications", "n_finite", "median", "range", "sd",
"table", "guardrail". It contains sensitivity of an estimand across
declared prior specifications and associated metadata or diagnostics
needed to interpret the result.
