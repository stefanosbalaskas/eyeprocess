# Canonicalise parameter-recovery results

Canonicalise parameter-recovery results

## Usage

``` r
as_irt_recovery_results(results)
```

## Arguments

- results:

  Data frame with at least \`replicate\`, \`parameter\`, \`truth\`, and
  \`estimate\`; optional \`lower\`, \`upper\`, \`converged\`,
  \`scenario\`, \`engine\`, and \`failure_type\` columns are retained.

## Value

A data frame containing canonicalise parameter-recovery results. Rows
represent the analysis units and columns contain the identifiers,
estimates, or diagnostics defined by the function.
