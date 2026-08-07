\name{detect_process_changepoints}
\alias{detect_process_changepoints}
\alias{segment_process_episodes}
\alias{label_process_episodes}
\alias{compare_episode_structure}
\alias{plot.eye_process_changepoints}
\alias{plot.eye_process_episodes}
\alias{plot.eye_episode_comparison}
\alias{plot_process_episodes}
\alias{plot_changepoint_ribbons}
\alias{plot_episode_waterfall}
\alias{plot_episode_transition_graph}
\alias{plot_episode_duration_distribution}
\title{Cognitive-episode change-point detection}
\description{
Cognitive-episode change-point detection. These functions form the eyeprocess 0.6.0.9000 measurement-intelligence programme and use dependency-free reference implementations with explicit evidence limits.
}
\usage{
detect_process_changepoints(x, channels = c("gaze_velocity", "aoi", "pupil", "eda"),
  time_col = NULL, window = 10, threshold_quantile = 0.9, min_segment = 5)
segment_process_episodes(x, ...)
label_process_episodes(x, rules = NULL, model = NULL)
compare_episode_structure(x, group)
plot_process_episodes(x, ...)
plot_changepoint_ribbons(x, ...)
plot_episode_waterfall(x, ...)
plot_episode_transition_graph(x, ...)
plot_episode_duration_distribution(x, ...)
}
\arguments{
\item{x}{Input object or data structure appropriate for the selected analysis.}
\item{channels}{Argument controlling `channels`; see the function usage and returned audit metadata.}
\item{time_col}{Argument controlling `time_col`; see the function usage and returned audit metadata.}
\item{window}{Argument controlling `window`; see the function usage and returned audit metadata.}
\item{threshold_quantile}{Argument controlling `threshold_quantile`; see the function usage and returned audit metadata.}
\item{min_segment}{Argument controlling `min_segment`; see the function usage and returned audit metadata.}
\item{...}{Additional arguments passed to the underlying method or plotting function.}
\item{rules}{Argument controlling `rules`; see the function usage and returned audit metadata.}
\item{model}{Argument controlling `model`; see the function usage and returned audit metadata.}
\item{group}{Argument controlling `group`; see the function usage and returned audit metadata.}
}
\details{
The APIs return auditable S3 objects. Plot wrappers call registered base-graphics methods. Experimental or approximate engines are labelled in object status fields and should be validated before confirmatory or operational use.
}
\value{
An eyeprocess result object, data frame, model object, plot, or audit table as documented by the individual function.
}
\seealso{\code{plot_diagnostics()}, \code{plot_evidence()}, and \code{plot_sensitivity()}.}
\keyword{methods}
