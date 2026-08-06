\name{validation_thresholds}
\alias{validation_thresholds}
\title{Specify completion and scientific-promotion thresholds}
\usage{
validation_thresholds(required_replications = 100L, max_failure_rate = 0.05,
  max_absolute_bias = 0.10, max_rmse = Inf, min_coverage = 0.90, max_coverage = 0.99,
  max_rhat = 1.01, min_ess_bulk = 400, max_divergence_rate = 0.01, require_sbc = TRUE,
  require_empirical_reproduction = TRUE)
}
\arguments{
  \item{required_replications}{Required completed replications per scenario.}
  \item{max_failure_rate}{Maximum tolerated job failure rate.}
  \item{max_absolute_bias}{Maximum absolute mean bias.}
  \item{max_rmse}{Maximum RMSE.}
  \item{min_coverage}{Minimum interval coverage.}
  \item{max_coverage}{Maximum interval coverage.}
  \item{max_rhat}{Maximum acceptable R-hat.}
  \item{min_ess_bulk}{Minimum bulk effective sample size.}
  \item{max_divergence_rate}{Maximum divergence rate.}
  \item{require_sbc}{Require passing SBC uniformity evidence.}
  \item{require_empirical_reproduction}{Require empirical-reproduction evidence.}
}
\value{The documented eyeprocess object, data frame, plot, report path, or adapter result.}
\description{Part of the research-scale validation, advanced-model, interoperability, storage, adapter, or reproducibility programme. Experimental model functions remain subject to declared evidence gates.}
