# Probabilistic AOIs and Compositional Attention

This article replaces brittle inside/outside AOI assignment with
probabilistic membership and then analyses AOI dwell allocation as a
composition. AOI probabilities remain explicit, overlap and boundary
ambiguity are audited, and downstream dwell, TTFF, transition, and
entropy metrics can be propagated through Monte Carlo draws.

``` r

aois <- data.frame(
  aoi_id = c("prompt", "evidence", "options"),
  xmin = c(0.05, 0.35, 0.10), xmax = c(0.30, 0.90, 0.90),
  ymin = c(0.05, 0.05, 0.60), ymax = c(0.45, 0.45, 0.95)
)
prob <- assign_aois_probabilistic(samples, aois, precision = c(0.03, 0.04))
plot_aoi_probability_map(prob)
plot_aoi_boundary_risk(audit_aoi_separation(prob))
uncertain_metrics <- propagate_aoi_uncertainty(
  prob, draws = 500, time_col = "time", duration_col = "duration"
)
plot_aoi_metric_uncertainty(uncertain_metrics)
```

For trial-level dwell totals, use log-ratio coordinates rather than
several raw proportions in the same ordinary regression.

``` r

composition <- derive_aoi_composition(
  trial_features,
  aois = c("prompt_dwell", "evidence_dwell", "options_dwell"),
  id_cols = c("person_id", "item_id", "condition")
)
ilr <- transform_aoi_composition(composition, "ilr")
comparison <- compare_aoi_compositions(composition, group = "condition")
plot_aoi_ternary(composition)
plot_aoi_variation_matrix(composition)
plot_compositional_group_difference(comparison)
```

The probability model and zero-replacement method must be reported.
Probabilistic assignment reduces false certainty; it does not remove
calibration error.
