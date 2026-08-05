\name{functional_pupil_basis}
\alias{functional_pupil_basis}
\title{Construct a functional basis for pupil trajectories}
\usage{
functional_pupil_basis(x, df = 6L, basis = c("natural_spline", "bspline"),
  degree = 3L, boundary_knots = NULL, knots = NULL)
}
\arguments{
  \item{x}{Prepared functional pupil data or numeric time vector.}
  \item{df}{Degrees of freedom.}
  \item{basis}{Natural spline or B-spline.}
  \item{degree}{B-spline polynomial degree; ignored for natural splines.}
  \item{boundary_knots}{Optional boundary knots.}
  \item{knots}{Optional internal knots.}
}
\value{The documented eyeprocess object, data frame, plot, report path, or adapter result.}
\description{Part of the research-scale validation, advanced-model, interoperability, storage, adapter, or reproducibility programme. Experimental model functions remain subject to declared evidence gates.}
