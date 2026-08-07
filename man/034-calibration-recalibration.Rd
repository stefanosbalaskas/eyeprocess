\name{detect_calibration_drift}
\alias{detect_calibration_drift}
\alias{fit_offline_recalibration}
\alias{apply_offline_recalibration}
\alias{audit_recalibration}
\alias{plot.eye_calibration_drift}
\alias{plot.eye_recalibration_audit}
\alias{plot_calibration_vector_field}
\alias{plot_calibration_error_ellipses}
\alias{plot_drift_over_time}
\alias{plot_recalibration_before_after}
\alias{plot_screen_coverage}
\title{Calibration drift and offline recalibration}
\description{
Calibration drift and offline recalibration. These functions form the eyeprocess 0.6.0.9000 measurement-intelligence programme and use dependency-free reference implementations with explicit evidence limits.
}
\usage{
detect_calibration_drift(x, references = NULL, window = "30 sec",
  method = c("targets", "known_aois", "fixation_density"), x_col = NULL,
  y_col = NULL, time_col = NULL)
fit_offline_recalibration(x, method = c("translation", "affine", "polynomial"),
  robust = TRUE, x_col = NULL, y_col = NULL, reference_x_col = NULL,
  reference_y_col = NULL)
apply_offline_recalibration(x, model, x_col = NULL, y_col = NULL,
  suffix = "_recalibrated")
audit_recalibration(before, after, minimum_improvement = NULL)
plot_calibration_vector_field(x, ...)
plot_calibration_error_ellipses(x, ...)
plot_drift_over_time(x, ...)
plot_recalibration_before_after(x, ...)
plot_screen_coverage(x, ...)
}
\arguments{
\item{x}{Input object or data structure appropriate for the selected analysis.}
\item{references}{Argument controlling `references`; see the function usage and returned audit metadata.}
\item{window}{Argument controlling `window`; see the function usage and returned audit metadata.}
\item{method}{Argument controlling `method`; see the function usage and returned audit metadata.}
\item{x_col}{Argument controlling `x_col`; see the function usage and returned audit metadata.}
\item{y_col}{Argument controlling `y_col`; see the function usage and returned audit metadata.}
\item{time_col}{Argument controlling `time_col`; see the function usage and returned audit metadata.}
\item{robust}{Argument controlling `robust`; see the function usage and returned audit metadata.}
\item{reference_x_col}{Argument controlling `reference_x_col`; see the function usage and returned audit metadata.}
\item{reference_y_col}{Argument controlling `reference_y_col`; see the function usage and returned audit metadata.}
\item{model}{Argument controlling `model`; see the function usage and returned audit metadata.}
\item{suffix}{Argument controlling `suffix`; see the function usage and returned audit metadata.}
\item{before}{Argument controlling `before`; see the function usage and returned audit metadata.}
\item{after}{Argument controlling `after`; see the function usage and returned audit metadata.}
\item{minimum_improvement}{Argument controlling `minimum_improvement`; see the function usage and returned audit metadata.}
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
