# Evidence and Decision Provenance

The evidence graph links raw data, transformations, metrics, models,
diagnostics, and decisions. The graph is deliberately explicit: its
edges document declared dependencies rather than inferring causal
relationships.

``` r

graph <- build_evidence_graph(
  raw_data = c("gaze_samples", "pupil_samples"),
  transformations = c("blink_removal", "baseline_correction", "probabilistic_AOI_assignment"),
  metrics = c("pupil_auc", "AOI_entropy"),
  models = c("functional_pupil_model", "item_process_model"),
  diagnostics = c("high_effort_evidence", "low_discrimination_evidence"),
  decisions = c("item_I17_revise")
)
plot_evidence_graph(graph)
trace <- trace_item_decision(graph, "I17")
plot_item_decision_path(trace)
audit_evidence_dependencies(graph)
```

Graph comparison reveals exactly which evidence or dependencies changed
between analysis versions.

``` r

comparison <- compare_decision_provenance(graph_v1, graph_v2)
plot(comparison)
plot_metric_dependency_graph(graph_v2)
plot_model_decision_impact(graph_v2)
```
