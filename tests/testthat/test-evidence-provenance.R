test_that("evidence graphs trace and audit decisions", {
  graph <- build_evidence_graph(
    raw_data = c("pupil_samples", "gaze_samples"),
    transformations = c("blink_removal", "baseline_correction"),
    metrics = c("pupil_auc", "aoi_entropy"),
    models = c("functional_pupil_model"),
    diagnostics = c("high_effort_evidence"),
    decisions = c("item_I01_revise")
  )
  expect_s3_class(graph, "eye_evidence_graph")
  trace <- trace_item_decision(graph, "I01")
  expect_s3_class(trace, "eye_decision_trace")
  audit <- audit_evidence_dependencies(graph)
  expect_true(audit$summary$passed)
  comparison <- compare_decision_provenance(graph, graph)
  expect_equal(sum(comparison$summary$count), 0)
  expect_plot_silent(plot_evidence_graph(graph))
})
