# Evidence and decision provenance graphs

Evidence and decision provenance graphs. These functions form the
eyeprocess 0.6.0.9000 measurement-intelligence programme and use
dependency-free reference implementations with explicit evidence limits.

## Usage

``` r
build_evidence_graph(raw_data, transformations = NULL, metrics = NULL,
  models = NULL, diagnostics = NULL, decisions = NULL, edges = NULL)
trace_item_decision(graph, item_id)
compare_decision_provenance(graph_a, graph_b)
audit_evidence_dependencies(graph)
plot_evidence_graph(x, ...)
plot_item_decision_path(x, ...)
plot_metric_dependency_graph(x, ...)
plot_model_decision_impact(x, ...)
```

## Arguments

- raw_data:

  Argument controlling \`raw_data\`; see the function usage and returned
  audit metadata.

- transformations:

  Argument controlling \`transformations\`; see the function usage and
  returned audit metadata.

- metrics:

  Argument controlling \`metrics\`; see the function usage and returned
  audit metadata.

- models:

  Argument controlling \`models\`; see the function usage and returned
  audit metadata.

- diagnostics:

  Argument controlling \`diagnostics\`; see the function usage and
  returned audit metadata.

- decisions:

  Argument controlling \`decisions\`; see the function usage and
  returned audit metadata.

- edges:

  Argument controlling \`edges\`; see the function usage and returned
  audit metadata.

- graph:

  Argument controlling \`graph\`; see the function usage and returned
  audit metadata.

- item_id:

  Argument controlling \`item_id\`; see the function usage and returned
  audit metadata.

- graph_a:

  Argument controlling \`graph_a\`; see the function usage and returned
  audit metadata.

- graph_b:

  Argument controlling \`graph_b\`; see the function usage and returned
  audit metadata.

- x:

  Input object or data structure appropriate for the selected analysis.

- ...:

  Additional arguments passed to the underlying method or plotting
  function.

## Details

The APIs return auditable S3 objects. Plot wrappers call registered
base-graphics methods. Experimental or approximate engines are labelled
in object status fields and should be validated before confirmatory or
operational use.

## Value

An eyeprocess result object, data frame, model object, plot, or audit
table as documented by the individual function.

## See also

[`plot_diagnostics()`](https://stefanosbalaskas.github.io/eyeprocess/reference/030-measurement-intelligence-utils.md),
[`plot_evidence()`](https://stefanosbalaskas.github.io/eyeprocess/reference/030-measurement-intelligence-utils.md),
and
[`plot_sensitivity()`](https://stefanosbalaskas.github.io/eyeprocess/reference/030-measurement-intelligence-utils.md).
