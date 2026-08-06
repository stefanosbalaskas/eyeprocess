\name{fit_dynamic_irtree}
\alias{fit_dynamic_irtree}
\title{Fit a dynamic gaze-state response-tree model}
\usage{
fit_dynamic_irtree(x, spec = dynamic_irtree_spec(), min_transitions = 10L, seed = 1L,
  ...)
}
\arguments{
  \item{x}{An `eye_dataset` or transition/long-state data frame.}
  \item{spec}{Dynamic IRTree specification.}
  \item{min_transitions}{Minimum support per destination for baseline logits.}
  \item{seed}{Random seed for probabilistic engines.}
  \item{...}{Engine-specific arguments.}
}
\value{The documented eyeprocess object, data frame, plot, report path, or adapter result.}
\description{Part of the research-scale validation, advanced-model, interoperability, storage, adapter, or reproducibility programme. Experimental model functions remain subject to declared evidence gates.}
