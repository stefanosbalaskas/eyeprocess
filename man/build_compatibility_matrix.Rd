\name{build_compatibility_matrix}
\alias{build_compatibility_matrix}
\title{Build the declared/fixture/empirical compatibility matrix}
\usage{
build_compatibility_matrix(x, required_vendors = c("gazepoint", "tobii", "pupillabs",
  "eyelink", "smi"), min_empirical_cases = 2L)
}
\arguments{
  \item{x}{Corpus path or registry data frame.}
  \item{required_vendors}{Required vendors.}
  \item{min_empirical_cases}{Minimum independent empirical cases per vendor.}
}
\value{The documented eyeprocess object, data frame, plot, report path, or adapter result.}
\description{Part of the research-scale validation, advanced-model, interoperability, storage, adapter, or reproducibility programme. Experimental model functions remain subject to declared evidence gates.}
