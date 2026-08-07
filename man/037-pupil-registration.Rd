\name{register_pupil_curves}
\alias{register_pupil_curves}
\alias{decompose_pupil_phase_amplitude}
\alias{fit_phase_amplitude_irt}
\alias{audit_pupil_registration}
\alias{plot.eye_pupil_registration}
\alias{plot.eye_pupil_phase_amplitude}
\alias{plot.eye_phase_amplitude_irt}
\alias{plot_pupil_registration}
\alias{plot_warping_functions}
\alias{plot_phase_amplitude_scores}
\alias{plot_item_phase_delay}
\alias{plot_registered_pupil_effects}
\title{Pupil phase-amplitude registration}
\description{
Pupil phase-amplitude registration. These functions form the eyeprocess 0.6.0.9000 measurement-intelligence programme and use dependency-free reference implementations with explicit evidence limits.
}
\usage{
register_pupil_curves(x, time, pupil, anchor = c("stimulus", "response", "event"),
  method = c("elastic", "landmark"), id_col = "person_id", grid_size = 101)
decompose_pupil_phase_amplitude(x, components = 3)
fit_phase_amplitude_irt(responses, phase_scores, amplitude_scores = NULL,
  person_id = NULL, family = c("gaussian", "binomial"), ...)
audit_pupil_registration(x)
plot_pupil_registration(x, ...)
plot_warping_functions(x, ...)
plot_phase_amplitude_scores(x, ...)
plot_item_phase_delay(x, ...)
plot_registered_pupil_effects(x, ...)
}
\arguments{
\item{x}{Input object or data structure appropriate for the selected analysis.}
\item{time}{Argument controlling `time`; see the function usage and returned audit metadata.}
\item{pupil}{Argument controlling `pupil`; see the function usage and returned audit metadata.}
\item{anchor}{Argument controlling `anchor`; see the function usage and returned audit metadata.}
\item{method}{Argument controlling `method`; see the function usage and returned audit metadata.}
\item{id_col}{Argument controlling `id_col`; see the function usage and returned audit metadata.}
\item{grid_size}{Argument controlling `grid_size`; see the function usage and returned audit metadata.}
\item{components}{Argument controlling `components`; see the function usage and returned audit metadata.}
\item{responses}{Argument controlling `responses`; see the function usage and returned audit metadata.}
\item{phase_scores}{Argument controlling `phase_scores`; see the function usage and returned audit metadata.}
\item{amplitude_scores}{Argument controlling `amplitude_scores`; see the function usage and returned audit metadata.}
\item{person_id}{Argument controlling `person_id`; see the function usage and returned audit metadata.}
\item{family}{Argument controlling `family`; see the function usage and returned audit metadata.}
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
