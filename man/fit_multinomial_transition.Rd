\name{fit_multinomial_transition}
\alias{fit_multinomial_transition}
\title{Fit a penalized multinomial transition model}
\usage{
fit_multinomial_transition(design, ridge = 1e-4, reference_state = NULL,
  control = list(maxit = 1000L, reltol = 1e-9))
}
\arguments{
  \item{design}{Transition design.}
  \item{ridge}{Ridge penalty.}
  \item{reference_state}{Reference destination state.}
  \item{control}{Passed to `optim()`.}
}
\value{The documented eyeprocess object, data frame, plot, report path, or adapter result.}
\description{Part of the research-scale validation, advanced-model, interoperability, storage, adapter, or reproducibility programme. Experimental model functions remain subject to declared evidence gates.}
