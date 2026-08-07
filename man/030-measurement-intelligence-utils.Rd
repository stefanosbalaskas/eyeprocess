\name{eye_plot_spec}
\alias{eye_plot_spec}
\alias{print.eye_plot_spec}
\alias{print.eye_mi_result}
\alias{plot_diagnostics}
\alias{plot_diagnostics.default}
\alias{plot_diagnostics.eye_mi_result}
\alias{plot_evidence}
\alias{plot_evidence.default}
\alias{plot_evidence.eye_mi_result}
\alias{plot_sensitivity}
\alias{plot_sensitivity.default}
\alias{plot_sensitivity.eye_mi_result}
\alias{autoplot_eyeprocess}
\title{Measurement-intelligence plotting and result infrastructure}
\description{
Measurement-intelligence plotting and result infrastructure. These functions form the eyeprocess 0.6.0.9000 measurement-intelligence programme and use dependency-free reference implementations with explicit evidence limits.
}
\usage{
eye_plot_spec(type = "default", title = NULL, xlab = NULL, ylab = NULL,
  caption = NULL, show_uncertainty = TRUE, show_raw = TRUE, facet_by = NULL,
  label_items = FALSE, interactive = FALSE)
plot_diagnostics(x, ...)
plot_evidence(x, ...)
plot_sensitivity(x, ...)
autoplot_eyeprocess(object, ...)
}
\arguments{
\item{type}{Argument controlling `type`; see the function usage and returned audit metadata.}
\item{title}{Argument controlling `title`; see the function usage and returned audit metadata.}
\item{xlab}{Argument controlling `xlab`; see the function usage and returned audit metadata.}
\item{ylab}{Argument controlling `ylab`; see the function usage and returned audit metadata.}
\item{caption}{Argument controlling `caption`; see the function usage and returned audit metadata.}
\item{show_uncertainty}{Argument controlling `show_uncertainty`; see the function usage and returned audit metadata.}
\item{show_raw}{Argument controlling `show_raw`; see the function usage and returned audit metadata.}
\item{facet_by}{Argument controlling `facet_by`; see the function usage and returned audit metadata.}
\item{label_items}{Argument controlling `label_items`; see the function usage and returned audit metadata.}
\item{interactive}{Argument controlling `interactive`; see the function usage and returned audit metadata.}
\item{x}{Input object or data structure appropriate for the selected analysis.}
\item{...}{Additional arguments passed to the underlying method or plotting function.}
\item{object}{Input object or data structure appropriate for the selected analysis.}
}
\details{
The APIs return auditable S3 objects. Plot wrappers call registered base-graphics methods. Experimental or approximate engines are labelled in object status fields and should be validated before confirmatory or operational use.
}
\value{
An eyeprocess result object, data frame, model object, plot, or audit table as documented by the individual function.
}
\seealso{\code{plot_diagnostics()}, \code{plot_evidence()}, and \code{plot_sensitivity()}.}
\keyword{methods}
