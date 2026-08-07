\name{item_objective_spec}
\alias{item_objective_spec}
\alias{item_pareto_front}
\alias{optimize_item_bank}
\alias{audit_bank_decision_stability}
\alias{plot.eye_item_pareto}
\alias{plot.eye_item_bank_optimization}
\alias{plot.eye_bank_decision_stability}
\alias{plot_item_pareto}
\alias{plot_objective_tradeoffs}
\alias{plot_bank_information_coverage}
\alias{plot_decision_stability}
\alias{plot_selected_bank_profile}
\title{Multi-objective item-bank optimization}
\description{
Multi-objective item-bank optimization. These functions form the eyeprocess 0.6.0.9000 measurement-intelligence programme and use dependency-free reference implementations with explicit evidence limits.
}
\usage{
item_objective_spec(information, process_burden, fairness, exposure,
  content_constraints = NULL, weights = c(information = 1, process_burden = 1,
  fairness = 1, exposure = 1), directions = c(information = "max",
  process_burden = "min", fairness = "min", exposure = "min"))
item_pareto_front(x, objectives)
optimize_item_bank(x, n_items, objectives, constraints = NULL, method = c("integer",
  "evolutionary"), iterations = 500, seed = 20260807)
audit_bank_decision_stability(x, draws = 1000, noise_sd = 0.1, seed = 20260807)
plot_item_pareto(x, ...)
plot_objective_tradeoffs(x, ...)
plot_bank_information_coverage(x, ...)
plot_decision_stability(x, ...)
plot_selected_bank_profile(x, ...)
}
\arguments{
\item{information}{Argument controlling `information`; see the function usage and returned audit metadata.}
\item{process_burden}{Argument controlling `process_burden`; see the function usage and returned audit metadata.}
\item{fairness}{Argument controlling `fairness`; see the function usage and returned audit metadata.}
\item{exposure}{Argument controlling `exposure`; see the function usage and returned audit metadata.}
\item{content_constraints}{Argument controlling `content_constraints`; see the function usage and returned audit metadata.}
\item{weights}{Argument controlling `weights`; see the function usage and returned audit metadata.}
\item{directions}{Argument controlling `directions`; see the function usage and returned audit metadata.}
\item{x}{Input object or data structure appropriate for the selected analysis.}
\item{objectives}{Argument controlling `objectives`; see the function usage and returned audit metadata.}
\item{n_items}{Argument controlling `n_items`; see the function usage and returned audit metadata.}
\item{constraints}{Argument controlling `constraints`; see the function usage and returned audit metadata.}
\item{method}{Argument controlling `method`; see the function usage and returned audit metadata.}
\item{iterations}{Argument controlling `iterations`; see the function usage and returned audit metadata.}
\item{seed}{Argument controlling `seed`; see the function usage and returned audit metadata.}
\item{draws}{Argument controlling `draws`; see the function usage and returned audit metadata.}
\item{noise_sd}{Argument controlling `noise_sd`; see the function usage and returned audit metadata.}
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
