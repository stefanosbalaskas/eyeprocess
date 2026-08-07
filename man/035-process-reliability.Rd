\name{fit_process_gstudy}
\alias{fit_process_gstudy}
\alias{process_variance_components}
\alias{design_process_dstudy}
\alias{audit_process_reliability}
\alias{plot.eye_process_gstudy}
\alias{plot.eye_process_dstudy}
\alias{plot.eye_process_reliability_audit}
\alias{plot_variance_components}
\alias{plot_dependability_surface}
\alias{plot_reliability_by_metric}
\alias{plot_session_stability}
\alias{plot_item_sampling_reliability}
\title{Process reliability and Generalizability Theory}
\description{
Process reliability and Generalizability Theory. These functions form the eyeprocess 0.6.0.9000 measurement-intelligence programme and use dependency-free reference implementations with explicit evidence limits.
}
\usage{
fit_process_gstudy(x, metric, facets = c("person", "item", "session", "device"),
  design = c("crossed", "nested"))
process_variance_components(x)
design_process_dstudy(gstudy, persons = NULL, items = seq(5, 50, 5), sessions = 1:5,
  devices = 1)
audit_process_reliability(x, metrics, method = c("icc", "gtheory", "split_half",
  "bootstrap"), person_col = "person_id", item_col = "item_id", draws = 250)
plot_variance_components(x, ...)
plot_dependability_surface(x, ...)
plot_reliability_by_metric(x, ...)
plot_session_stability(x, ...)
plot_item_sampling_reliability(x, ...)
}
\arguments{
\item{x}{Input object or data structure appropriate for the selected analysis.}
\item{metric}{Argument controlling `metric`; see the function usage and returned audit metadata.}
\item{facets}{Argument controlling `facets`; see the function usage and returned audit metadata.}
\item{design}{Argument controlling `design`; see the function usage and returned audit metadata.}
\item{gstudy}{Argument controlling `gstudy`; see the function usage and returned audit metadata.}
\item{persons}{Argument controlling `persons`; see the function usage and returned audit metadata.}
\item{items}{Argument controlling `items`; see the function usage and returned audit metadata.}
\item{sessions}{Argument controlling `sessions`; see the function usage and returned audit metadata.}
\item{devices}{Argument controlling `devices`; see the function usage and returned audit metadata.}
\item{metrics}{Argument controlling `metrics`; see the function usage and returned audit metadata.}
\item{method}{Argument controlling `method`; see the function usage and returned audit metadata.}
\item{person_col}{Argument controlling `person_col`; see the function usage and returned audit metadata.}
\item{item_col}{Argument controlling `item_col`; see the function usage and returned audit metadata.}
\item{draws}{Argument controlling `draws`; see the function usage and returned audit metadata.}
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
