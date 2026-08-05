\name{fit_strategy_mixture_em}
\alias{fit_strategy_mixture_em}
\title{Fit the deterministic multi-start EM baseline}
\usage{
fit_strategy_mixture_em(prepared, starts = prepared$spec$multiple_starts,
  max_iter = 300L, tolerance = 1e-7, seed = 1L)
}
\arguments{
  \item{prepared}{Prepared strategy data.}
  \item{starts}{Number of starts.}
  \item{max_iter}{Maximum EM iterations.}
  \item{tolerance}{Relative log-likelihood tolerance.}
  \item{seed}{Seed.}
}
\value{The documented eyeprocess object, data frame, plot, report path, or adapter result.}
\description{Part of the research-scale validation, advanced-model, interoperability, storage, adapter, or reproducibility programme. Experimental model functions remain subject to declared evidence gates.}
