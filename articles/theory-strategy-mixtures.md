# Theory-constrained strategy mixtures

## Prespecification before fitting

A strategy mixture is appropriate only when theory defines
distinguishable process signatures before estimation. Data-derived
classes must not be named after cognition merely because their means
differ.

``` r

spec <- theory_strategy_spec(
  strategies = list(
    analytic = c(prompt_dwell = 1, evidence_dwell = 1, option_switches = 0.5),
    heuristic = c(prompt_dwell = -0.5, evidence_dwell = -0.8, option_switches = -0.2)
  ),
  response = "score",
  participant = "participant_id",
  item = "item_id",
  condition = "condition",
  item_availability = availability,
  engine = "stan",
  anchor_strength = 3
)

fit <- fit_theory_strategy_irt(trials, spec, seed = 42)
```

## Classification uncertainty

``` r

probability <- strategy_posterior_probabilities(fit)
strategy_classification_uncertainty(fit, threshold = 0.70)
strategy_label_switching_diagnostics(fit)
```

Posterior probabilities and entropy are primary outputs. Modal
assignment alone conceals uncertainty.

## Sensitivity and competing heterogeneity

``` r

sensitivity <- strategy_aoi_sensitivity(
  list(primary_aoi = trials_primary, expanded_aoi = trials_expanded),
  spec,
  seed = 42
)
plot(sensitivity)
compare_strategy_heterogeneity(fit)
```

The package compares the discrete mixture with a continuous
process-heterogeneity model and requires external strategy manipulations
before substantive class labels can be promoted.
