\name{derive_aoi_composition}
\alias{derive_aoi_composition}
\alias{transform_aoi_composition}
\alias{fit_aoi_compositional_model}
\alias{compare_aoi_compositions}
\alias{aoi_balance_coordinates}
\alias{plot.eye_aoi_composition}
\alias{plot.eye_aoi_composition_comparison}
\alias{plot.eye_aoi_composition_model}
\alias{plot_aoi_ternary}
\alias{plot_aoi_balance_biplot}
\alias{plot_aoi_variation_matrix}
\alias{plot_compositional_group_difference}
\alias{plot_aoi_composition_trajectory}
\title{Compositional analysis of AOI attention}
\description{
Compositional analysis of AOI attention. These functions form the eyeprocess 0.6.0.9000 measurement-intelligence programme and use dependency-free reference implementations with explicit evidence limits.
}
\usage{
derive_aoi_composition(x, aois, denominator = c("total_aoi_dwell",
  "trial_duration"), zero_method = c("multiplicative", "bayesian"), id_cols = NULL,
  aoi_col = "aoi", value_col = "dwell_ms", trial_duration_col = NULL)
transform_aoi_composition(x, method = c("ilr", "clr", "alr"), reference = NULL)
fit_aoi_compositional_model(composition, formula, random = NULL, data = NULL,
  method = "ilr")
compare_aoi_compositions(x, group, method = c("permanova", "compositional_manova"),
  permutations = 499, seed = 20260807)
aoi_balance_coordinates(x, balances)
plot_aoi_ternary(x, ...)
plot_aoi_balance_biplot(x, ...)
plot_aoi_variation_matrix(x, ...)
plot_compositional_group_difference(x, ...)
plot_aoi_composition_trajectory(x, ...)
}
\arguments{
\item{x}{Input object or data structure appropriate for the selected analysis.}
\item{aois}{Argument controlling `aois`; see the function usage and returned audit metadata.}
\item{denominator}{Argument controlling `denominator`; see the function usage and returned audit metadata.}
\item{zero_method}{Argument controlling `zero_method`; see the function usage and returned audit metadata.}
\item{id_cols}{Argument controlling `id_cols`; see the function usage and returned audit metadata.}
\item{aoi_col}{Argument controlling `aoi_col`; see the function usage and returned audit metadata.}
\item{value_col}{Argument controlling `value_col`; see the function usage and returned audit metadata.}
\item{trial_duration_col}{Argument controlling `trial_duration_col`; see the function usage and returned audit metadata.}
\item{method}{Argument controlling `method`; see the function usage and returned audit metadata.}
\item{reference}{Argument controlling `reference`; see the function usage and returned audit metadata.}
\item{composition}{Argument controlling `composition`; see the function usage and returned audit metadata.}
\item{formula}{Argument controlling `formula`; see the function usage and returned audit metadata.}
\item{random}{Argument controlling `random`; see the function usage and returned audit metadata.}
\item{data}{Argument controlling `data`; see the function usage and returned audit metadata.}
\item{group}{Argument controlling `group`; see the function usage and returned audit metadata.}
\item{permutations}{Argument controlling `permutations`; see the function usage and returned audit metadata.}
\item{seed}{Argument controlling `seed`; see the function usage and returned audit metadata.}
\item{balances}{Argument controlling `balances`; see the function usage and returned audit metadata.}
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
