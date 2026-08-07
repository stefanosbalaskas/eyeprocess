\name{representative_scanpath}
\alias{representative_scanpath}
\alias{scanpath_dispersion}
\alias{compare_scanpath_distributions}
\alias{bootstrap_representative_scanpath}
\alias{plot.eye_scanpath_representative}
\alias{plot.eye_scanpath_comparison}
\alias{plot.eye_scanpath_bootstrap}
\alias{plot_scanpath_atlas}
\alias{plot_representative_scanpath}
\alias{plot_scanpath_dispersion}
\alias{plot_group_scanpath_transport}
\alias{plot_scanpath_similarity_matrix}
\title{Representative scanpaths and scanpath distributions}
\description{
Representative scanpaths and scanpath distributions. These functions form the eyeprocess 0.6.0.9000 measurement-intelligence programme and use dependency-free reference implementations with explicit evidence limits.
}
\usage{
representative_scanpath(x, method = c("medoid", "barycenter", "consensus"),
  id_col = "person_id", aoi_col = "aoi", x_col = "x", y_col = "y",
  distance = c("multimatch", "edit", "transport"))
scanpath_dispersion(x)
compare_scanpath_distributions(x, group, distance = c("multimatch", "edit",
  "transport"), permutations = 499)
bootstrap_representative_scanpath(x, draws = 250, seed = 20260807)
plot_scanpath_atlas(x, ...)
plot_representative_scanpath(x, ...)
plot_scanpath_dispersion(x, ...)
plot_group_scanpath_transport(x, ...)
plot_scanpath_similarity_matrix(x, ...)
}
\arguments{
\item{x}{Input object or data structure appropriate for the selected analysis.}
\item{method}{Argument controlling `method`; see the function usage and returned audit metadata.}
\item{id_col}{Argument controlling `id_col`; see the function usage and returned audit metadata.}
\item{aoi_col}{Argument controlling `aoi_col`; see the function usage and returned audit metadata.}
\item{x_col}{Argument controlling `x_col`; see the function usage and returned audit metadata.}
\item{y_col}{Argument controlling `y_col`; see the function usage and returned audit metadata.}
\item{distance}{Argument controlling `distance`; see the function usage and returned audit metadata.}
\item{group}{Argument controlling `group`; see the function usage and returned audit metadata.}
\item{permutations}{Argument controlling `permutations`; see the function usage and returned audit metadata.}
\item{draws}{Argument controlling `draws`; see the function usage and returned audit metadata.}
\item{seed}{Argument controlling `seed`; see the function usage and returned audit metadata.}
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
