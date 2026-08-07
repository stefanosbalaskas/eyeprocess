\name{assign_aois_probabilistic}
\alias{assign_aois_probabilistic}
\alias{audit_aoi_separation}
\alias{summarise_aoi_membership}
\alias{propagate_aoi_uncertainty}
\alias{plot.eye_probabilistic_aoi}
\alias{plot.eye_aoi_separation_audit}
\alias{plot.eye_aoi_uncertainty}
\alias{plot_aoi_probability_map}
\alias{plot_aoi_boundary_risk}
\alias{plot_probabilistic_scanpath}
\alias{plot_fuzzy_transition_matrix}
\alias{plot_aoi_metric_uncertainty}
\title{Probabilistic AOI assignment and uncertainty propagation}
\description{
Probabilistic AOI assignment and uncertainty propagation. These functions form the eyeprocess 0.6.0.9000 measurement-intelligence programme and use dependency-free reference implementations with explicit evidence limits.
}
\usage{
assign_aois_probabilistic(x, aois, error_model = c("empirical", "gaussian",
  "ellipse"), accuracy = NULL, precision = NULL, x_col = NULL, y_col = NULL,
  id_cols = NULL)
audit_aoi_separation(x = NULL, aois = NULL)
summarise_aoi_membership(x, by = NULL)
propagate_aoi_uncertainty(x, metrics = c("dwell", "ttff", "transitions", "entropy"),
  draws = 500, time_col = NULL, duration_col = NULL, seed = 20260807)
plot_aoi_probability_map(x, ...)
plot_aoi_boundary_risk(x, ...)
plot_probabilistic_scanpath(x, ...)
plot_fuzzy_transition_matrix(x, ...)
plot_aoi_metric_uncertainty(x, ...)
}
\arguments{
\item{x}{Input object or data structure appropriate for the selected analysis.}
\item{aois}{Argument controlling `aois`; see the function usage and returned audit metadata.}
\item{error_model}{Argument controlling `error_model`; see the function usage and returned audit metadata.}
\item{accuracy}{Argument controlling `accuracy`; see the function usage and returned audit metadata.}
\item{precision}{Argument controlling `precision`; see the function usage and returned audit metadata.}
\item{x_col}{Argument controlling `x_col`; see the function usage and returned audit metadata.}
\item{y_col}{Argument controlling `y_col`; see the function usage and returned audit metadata.}
\item{id_cols}{Argument controlling `id_cols`; see the function usage and returned audit metadata.}
\item{by}{Argument controlling `by`; see the function usage and returned audit metadata.}
\item{metrics}{Argument controlling `metrics`; see the function usage and returned audit metadata.}
\item{draws}{Argument controlling `draws`; see the function usage and returned audit metadata.}
\item{time_col}{Argument controlling `time_col`; see the function usage and returned audit metadata.}
\item{duration_col}{Argument controlling `duration_col`; see the function usage and returned audit metadata.}
\item{seed}{Argument controlling `seed`; see the function usage and returned audit metadata.}
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
