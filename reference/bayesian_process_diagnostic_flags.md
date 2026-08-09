# Extract compact Bayesian process-model diagnostic flags

Extract compact Bayesian process-model diagnostic flags

## Usage

``` r
bayesian_process_diagnostic_flags(
  x,
  rhat_threshold = 1.01,
  ess_threshold = 400
)
```

## Arguments

- x:

  An \`eye_bayesian_process_dashboard\`.

- rhat_threshold:

  Review threshold for R-hat.

- ess_threshold:

  Review threshold for bulk/tail effective sample size.
