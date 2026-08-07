\name{fit_process_observation_model}
\alias{fit_process_observation_model}
\alias{fit_joint_signal_missingness}
\alias{process_pattern_mixture}
\alias{sensitivity_mnar_process}
\alias{plot.eye_process_observation_model}
\alias{plot.eye_mnar_sensitivity}
\alias{plot.eye_mnar_tipping_point}
\alias{plot_observation_probability}
\alias{plot_missingness_by_time}
\alias{plot_missingness_by_aoi}
\alias{plot_mnar_tipping_point}
\alias{plot_complete_case_sensitivity}
\title{Informative missingness and MNAR sensitivity}
\description{
Informative missingness and MNAR sensitivity. These functions form the eyeprocess 0.6.0.9000 measurement-intelligence programme and use dependency-free reference implementations with explicit evidence limits.
}
\usage{
fit_process_observation_model(x, observed, predictors, random = c("person", "item"))
fit_joint_signal_missingness(outcome, observation, method = c("selection",
  "shared_parameter"), x = NULL, predictors = NULL)
process_pattern_mixture(x, delta = seq(-1, 1, 0.1), metric = NULL, estimand = mean)
sensitivity_mnar_process(x, estimand = mean, null = 0, ...)
plot_observation_probability(x, ...)
plot_missingness_by_time(x, ...)
plot_missingness_by_aoi(x, ...)
plot_mnar_tipping_point(x, ...)
plot_complete_case_sensitivity(x, ...)
}
\arguments{
\item{x}{Input object or data structure appropriate for the selected analysis.}
\item{observed}{Argument controlling `observed`; see the function usage and returned audit metadata.}
\item{predictors}{Argument controlling `predictors`; see the function usage and returned audit metadata.}
\item{random}{Argument controlling `random`; see the function usage and returned audit metadata.}
\item{outcome}{Argument controlling `outcome`; see the function usage and returned audit metadata.}
\item{observation}{Argument controlling `observation`; see the function usage and returned audit metadata.}
\item{method}{Argument controlling `method`; see the function usage and returned audit metadata.}
\item{delta}{Argument controlling `delta`; see the function usage and returned audit metadata.}
\item{metric}{Argument controlling `metric`; see the function usage and returned audit metadata.}
\item{estimand}{Argument controlling `estimand`; see the function usage and returned audit metadata.}
\item{null}{Argument controlling `null`; see the function usage and returned audit metadata.}
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
