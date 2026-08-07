\name{process_uncertainty_spec}
\alias{process_uncertainty_spec}
\alias{print.eye_process_uncertainty_spec}
\alias{estimate_process_uncertainty}
\alias{propagate_process_uncertainty}
\alias{uncertainty_budget}
\alias{compare_uncertainty_budgets}
\alias{plot.eye_process_uncertainty}
\alias{plot.eye_process_uncertainty_propagation}
\alias{plot.eye_uncertainty_budget_comparison}
\alias{plot_uncertainty_waterfall}
\alias{plot_uncertainty_tornado}
\alias{plot_uncertainty_by_item}
\alias{plot_uncertainty_by_stage}
\title{Process measurement-uncertainty budgets}
\description{
Process measurement-uncertainty budgets. These functions form the eyeprocess 0.6.0.9000 measurement-intelligence programme and use dependency-free reference implementations with explicit evidence limits.
}
\usage{
process_uncertainty_spec(calibration = TRUE, aoi_assignment = TRUE,
  preprocessing = TRUE, sampling = TRUE, model = TRUE, source_sd = NULL,
  draws = 1000, seed = 20260807)
estimate_process_uncertainty(x, spec = process_uncertainty_spec(), metrics = NULL,
  cluster = NULL)
propagate_process_uncertainty(x, estimand = function(data) mean(data, na.rm = TRUE),
  method = c("bootstrap", "simulation", "posterior"), draws = NULL, seed = NULL)
uncertainty_budget(x)
compare_uncertainty_budgets(...)
plot_uncertainty_waterfall(x, ...)
plot_uncertainty_tornado(x, ...)
plot_uncertainty_by_item(x, ...)
plot_uncertainty_by_stage(x, ...)
}
\arguments{
\item{calibration}{Argument controlling `calibration`; see the function usage and returned audit metadata.}
\item{aoi_assignment}{Argument controlling `aoi_assignment`; see the function usage and returned audit metadata.}
\item{preprocessing}{Argument controlling `preprocessing`; see the function usage and returned audit metadata.}
\item{sampling}{Argument controlling `sampling`; see the function usage and returned audit metadata.}
\item{model}{Argument controlling `model`; see the function usage and returned audit metadata.}
\item{source_sd}{Argument controlling `source_sd`; see the function usage and returned audit metadata.}
\item{draws}{Argument controlling `draws`; see the function usage and returned audit metadata.}
\item{seed}{Argument controlling `seed`; see the function usage and returned audit metadata.}
\item{x}{Input object or data structure appropriate for the selected analysis.}
\item{spec}{Argument controlling `spec`; see the function usage and returned audit metadata.}
\item{metrics}{Argument controlling `metrics`; see the function usage and returned audit metadata.}
\item{cluster}{Argument controlling `cluster`; see the function usage and returned audit metadata.}
\item{estimand}{Argument controlling `estimand`; see the function usage and returned audit metadata.}
\item{method}{Argument controlling `method`; see the function usage and returned audit metadata.}
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
