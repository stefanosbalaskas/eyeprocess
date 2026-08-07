\name{build_evidence_graph}
\alias{build_evidence_graph}
\alias{trace_item_decision}
\alias{compare_decision_provenance}
\alias{audit_evidence_dependencies}
\alias{plot.eye_evidence_graph}
\alias{plot.eye_decision_trace}
\alias{plot.eye_provenance_comparison}
\alias{plot_evidence_graph}
\alias{plot_item_decision_path}
\alias{plot_metric_dependency_graph}
\alias{plot_model_decision_impact}
\title{Evidence and decision provenance graphs}
\description{
Evidence and decision provenance graphs. These functions form the eyeprocess 0.6.0.9000 measurement-intelligence programme and use dependency-free reference implementations with explicit evidence limits.
}
\usage{
build_evidence_graph(raw_data, transformations = NULL, metrics = NULL,
  models = NULL, diagnostics = NULL, decisions = NULL, edges = NULL)
trace_item_decision(graph, item_id)
compare_decision_provenance(graph_a, graph_b)
audit_evidence_dependencies(graph)
plot_evidence_graph(x, ...)
plot_item_decision_path(x, ...)
plot_metric_dependency_graph(x, ...)
plot_model_decision_impact(x, ...)
}
\arguments{
\item{raw_data}{Argument controlling `raw_data`; see the function usage and returned audit metadata.}
\item{transformations}{Argument controlling `transformations`; see the function usage and returned audit metadata.}
\item{metrics}{Argument controlling `metrics`; see the function usage and returned audit metadata.}
\item{models}{Argument controlling `models`; see the function usage and returned audit metadata.}
\item{diagnostics}{Argument controlling `diagnostics`; see the function usage and returned audit metadata.}
\item{decisions}{Argument controlling `decisions`; see the function usage and returned audit metadata.}
\item{edges}{Argument controlling `edges`; see the function usage and returned audit metadata.}
\item{graph}{Argument controlling `graph`; see the function usage and returned audit metadata.}
\item{item_id}{Argument controlling `item_id`; see the function usage and returned audit metadata.}
\item{graph_a}{Argument controlling `graph_a`; see the function usage and returned audit metadata.}
\item{graph_b}{Argument controlling `graph_b`; see the function usage and returned audit metadata.}
\item{x}{Input object or data structure appropriate for the selected analysis.}
\item{...}{Additional arguments passed to the underlying method or plotting function.}
}
\details{
The APIs return auditable S3 objects. Plot wrappers call registered base-graphics methods. Experimental or approximate engines are labelled in object status fields and should be validated before confirmatory or operational use.
}
\value{
An eyeprocess result object, data frame, model object, plot, or audit table as documented by the individual function.
}
\seealso{\code{plot_diagnostics()}, \code{plot_evidence()}, and \code{plot_sensitivity()}.}
\keyword{methods}
