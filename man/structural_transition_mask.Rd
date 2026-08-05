\name{structural_transition_mask}
\alias{structural_transition_mask}
\title{Define structural-zero and allowed transition masks}
\usage{
structural_transition_mask(states, forbidden = NULL, allowed = NULL,
  allow_self = TRUE, structural_zeros = NULL, allowed_transitions = NULL)
}
\arguments{
  \item{states}{State labels.}
  \item{forbidden}{Two-column data frame/matrix of forbidden from-to pairs.}
  \item{allowed}{Two-column data frame/matrix of explicitly allowed pairs.}
  \item{allow_self}{Whether self transitions are allowed by default.}
  \item{structural_zeros}{Alias for `forbidden`.}
  \item{allowed_transitions}{Alias for `allowed`.}
}
\value{The documented eyeprocess object, data frame, plot, report path, or adapter result.}
\description{Part of the research-scale validation, advanced-model, interoperability, storage, adapter, or reproducibility programme. Experimental model functions remain subject to declared evidence gates.}
