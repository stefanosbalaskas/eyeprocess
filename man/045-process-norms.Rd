\name{fit_process_norms}
\alias{fit_process_norms}
\alias{predict_process_centiles}
\alias{score_process_deviation}
\alias{audit_norm_transportability}
\alias{plot.eye_process_norms}
\alias{plot.eye_norm_transportability}
\alias{plot_process_centiles}
\alias{plot_normative_fan}
\alias{plot_person_normative_profile}
\alias{plot_item_normative_deviation}
\title{Conditional process reference centiles}
\description{
Conditional process reference centiles. These functions form the eyeprocess 0.6.0.9000 measurement-intelligence programme and use dependency-free reference implementations with explicit evidence limits.
}
\usage{
fit_process_norms(x, metric, covariates, family = c("auto", "gaussian",
  "lognormal"))
predict_process_centiles(model, newdata, centiles = c(2.5, 10, 25, 50, 75, 90,
  97.5))
score_process_deviation(model, newdata, type = c("z", "centile",
  "tail_probability"))
audit_norm_transportability(model, new_sample)
plot_process_centiles(x, ...)
plot_normative_fan(x, ...)
plot_person_normative_profile(x, ...)
plot_item_normative_deviation(x, ...)
}
\arguments{
\item{x}{Input object or data structure appropriate for the selected analysis.}
\item{metric}{Argument controlling `metric`; see the function usage and returned audit metadata.}
\item{covariates}{Argument controlling `covariates`; see the function usage and returned audit metadata.}
\item{family}{Argument controlling `family`; see the function usage and returned audit metadata.}
\item{model}{Argument controlling `model`; see the function usage and returned audit metadata.}
\item{newdata}{Argument controlling `newdata`; see the function usage and returned audit metadata.}
\item{centiles}{Argument controlling `centiles`; see the function usage and returned audit metadata.}
\item{type}{Argument controlling `type`; see the function usage and returned audit metadata.}
\item{new_sample}{Argument controlling `new_sample`; see the function usage and returned audit metadata.}
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
