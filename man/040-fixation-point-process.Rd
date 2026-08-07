\name{fit_fixation_point_process}
\alias{fit_fixation_point_process}
\alias{fit_marked_gaze_process}
\alias{predict_fixation_intensity}
\alias{diagnose_gaze_point_process}
\alias{plot.eye_fixation_point_process}
\alias{plot.eye_gaze_point_process_diagnostics}
\alias{plot_fixation_intensity}
\alias{plot_spatial_residuals}
\alias{plot_temporal_excitation_kernel}
\alias{plot_covariate_effect_surface}
\alias{plot_observed_expected_fixations}
\title{Spatio-temporal fixation point-process models}
\description{
Spatio-temporal fixation point-process models. These functions form the eyeprocess 0.6.0.9000 measurement-intelligence programme and use dependency-free reference implementations with explicit evidence limits.
}
\usage{
fit_fixation_point_process(x, spatial_covariates = NULL, temporal_covariates = NULL,
  interaction = c("none", "self_exciting"), x_col = "x", y_col = "y",
  time_col = "time", grid_size = 20)
fit_marked_gaze_process(x, marks = c("duration", "pupil", "saccade_amplitude"),
  x_col = "x", y_col = "y", time_col = "time")
predict_fixation_intensity(model, new_stimulus = NULL)
diagnose_gaze_point_process(model)
plot_fixation_intensity(x, ...)
plot_spatial_residuals(x, ...)
plot_temporal_excitation_kernel(x, ...)
plot_covariate_effect_surface(x, ...)
plot_observed_expected_fixations(x, ...)
}
\arguments{
\item{x}{Input object or data structure appropriate for the selected analysis.}
\item{spatial_covariates}{Argument controlling `spatial_covariates`; see the function usage and returned audit metadata.}
\item{temporal_covariates}{Argument controlling `temporal_covariates`; see the function usage and returned audit metadata.}
\item{interaction}{Argument controlling `interaction`; see the function usage and returned audit metadata.}
\item{x_col}{Argument controlling `x_col`; see the function usage and returned audit metadata.}
\item{y_col}{Argument controlling `y_col`; see the function usage and returned audit metadata.}
\item{time_col}{Argument controlling `time_col`; see the function usage and returned audit metadata.}
\item{grid_size}{Argument controlling `grid_size`; see the function usage and returned audit metadata.}
\item{marks}{Argument controlling `marks`; see the function usage and returned audit metadata.}
\item{model}{Argument controlling `model`; see the function usage and returned audit metadata.}
\item{new_stimulus}{Argument controlling `new_stimulus`; see the function usage and returned audit metadata.}
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
