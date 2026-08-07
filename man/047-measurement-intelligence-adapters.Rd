\name{fit_process_missingness_model}
\alias{fit_process_missingness_model}
\alias{crossmodal_recurrence_model}
\alias{plot.eye_crossmodal_recurrence_model}
\title{Measurement-intelligence compatibility adapters}
\description{
Measurement-intelligence compatibility adapters. These functions form the eyeprocess 0.6.0.9000 measurement-intelligence programme and use dependency-free reference implementations with explicit evidence limits.
}
\usage{
fit_process_missingness_model(x, observed, predictors, random = c("person", "item"))
crossmodal_recurrence_model(x, y, outcome = NULL, channels = c("gaze_pupil",
  "gaze_eda", "pupil_eda"), radius = NULL, covariates = NULL)
}
\arguments{
\item{x}{Input object or data structure appropriate for the selected analysis.}
\item{observed}{Argument controlling `observed`; see the function usage and returned audit metadata.}
\item{predictors}{Argument controlling `predictors`; see the function usage and returned audit metadata.}
\item{random}{Argument controlling `random`; see the function usage and returned audit metadata.}
\item{y}{Argument controlling `y`; see the function usage and returned audit metadata.}
\item{outcome}{Argument controlling `outcome`; see the function usage and returned audit metadata.}
\item{channels}{Argument controlling `channels`; see the function usage and returned audit metadata.}
\item{radius}{Argument controlling `radius`; see the function usage and returned audit metadata.}
\item{covariates}{Argument controlling `covariates`; see the function usage and returned audit metadata.}
}
\details{
The APIs return auditable S3 objects. Plot wrappers call registered base-graphics methods. Experimental or approximate engines are labelled in object status fields and should be validated before confirmatory or operational use.
}
\value{
An eyeprocess result object, data frame, model object, plot, or audit table as documented by the individual function.
}
\seealso{\code{plot_diagnostics()}, \code{plot_evidence()}, and \code{plot_sensitivity()}.}
\keyword{methods}
