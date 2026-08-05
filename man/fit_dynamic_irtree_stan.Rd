\name{fit_dynamic_irtree_stan}
\alias{fit_dynamic_irtree_stan}
\title{Fit an optional CmdStan dynamic-transition model}
\usage{
fit_dynamic_irtree_stan(design, spec, seed = 1L, refresh = 0L, output_dir = NULL, ...)
}
\arguments{
  \item{design}{Transition design.}
  \item{spec}{Dynamic IRTree specification.}
  \item{seed}{Random seed.}
  \item{refresh}{CmdStan refresh interval.}
  \item{output_dir}{Optional CmdStan output directory.}
  \item{...}{Additional arguments to `CmdStanModel$sample()`.}
}
\value{The documented eyeprocess object, data frame, plot, report path, or adapter result.}
\description{Part of the research-scale validation, advanced-model, interoperability, storage, adapter, or reproducibility programme. Experimental model functions remain subject to declared evidence gates.}
