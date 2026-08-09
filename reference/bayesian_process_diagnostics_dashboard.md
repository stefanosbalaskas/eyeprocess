# Summarize Bayesian process-model diagnostics

Collects model availability, optional approximate leave-one-out
summaries, posterior diagnostics, and (only when explicitly requested) a
Bayes-factor comparison. The function does not treat any one diagnostic
as proof of the substantive process interpretation.

## Usage

``` r
bayesian_process_diagnostics_dashboard(
  ...,
  model_names = NULL,
  compute_loo = TRUE,
  compute_bayes_factor = FALSE,
  posterior_summary = TRUE
)
```

## Arguments

- ...:

  Fitted \`brmsfit\` objects.

- model_names:

  Optional model labels.

- compute_loo:

  Whether to compute approximate leave-one-out diagnostics.

- compute_bayes_factor:

  Whether to attempt a Bayes factor. This requires exactly two suitable
  brms models and typically models fitted with \`save_pars =
  save_pars(all = TRUE)\`.

- posterior_summary:

  Whether to collect posterior convergence summaries when package
  \`posterior\` is available.

## Value

An \`eye_bayesian_process_dashboard\` object.
