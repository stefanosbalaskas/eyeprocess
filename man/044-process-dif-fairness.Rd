\name{fit_process_dif}
\alias{fit_process_dif}
\alias{monitor_dif_drift}
\alias{decompose_dif_evidence}
\alias{audit_fairness_transportability}
\alias{plot.eye_process_dif}
\alias{plot.eye_dif_drift}
\alias{plot.eye_fairness_transportability}
\alias{plot_group_icc_process_overlay}
\alias{plot_process_dif_forest}
\alias{plot_dif_drift_heatmap}
\alias{plot_fairness_transport_matrix}
\alias{plot_item_group_process_curves}
\title{Dynamic process-DIF and fairness drift}
\description{
Dynamic process-DIF and fairness drift. These functions form the eyeprocess 0.6.0.9000 measurement-intelligence programme and use dependency-free reference implementations with explicit evidence limits.
}
\usage{
fit_process_dif(x, response, process, group, item, ability = NULL)
monitor_dif_drift(x, time, group, metrics, item = NULL)
decompose_dif_evidence(psychometric, process = NULL, design_features = NULL)
audit_fairness_transportability(x, context = "device", effect = "process_dif",
  item = "item_id")
plot_group_icc_process_overlay(x, ...)
plot_process_dif_forest(x, ...)
plot_dif_drift_heatmap(x, ...)
plot_fairness_transport_matrix(x, ...)
plot_item_group_process_curves(x, ...)
}
\arguments{
\item{x}{Input object or data structure appropriate for the selected analysis.}
\item{response}{Argument controlling `response`; see the function usage and returned audit metadata.}
\item{process}{Argument controlling `process`; see the function usage and returned audit metadata.}
\item{group}{Argument controlling `group`; see the function usage and returned audit metadata.}
\item{item}{Argument controlling `item`; see the function usage and returned audit metadata.}
\item{ability}{Argument controlling `ability`; see the function usage and returned audit metadata.}
\item{time}{Argument controlling `time`; see the function usage and returned audit metadata.}
\item{metrics}{Argument controlling `metrics`; see the function usage and returned audit metadata.}
\item{psychometric}{Argument controlling `psychometric`; see the function usage and returned audit metadata.}
\item{design_features}{Argument controlling `design_features`; see the function usage and returned audit metadata.}
\item{context}{Argument controlling `context`; see the function usage and returned audit metadata.}
\item{effect}{Argument controlling `effect`; see the function usage and returned audit metadata.}
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
