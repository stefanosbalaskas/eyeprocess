\name{validate_strategy_manipulation}
\alias{validate_strategy_manipulation}
\title{Validate strategy posteriors against an experimental manipulation}
\usage{
validate_strategy_manipulation(object, condition, expected_strategy,
  minimum_contrast = 0)
}
\arguments{
  \item{object}{Fitted strategy-mixture object.}
  \item{condition}{Condition column in the original trial data.}
  \item{expected_strategy}{Named character vector mapping condition values to prespecified strategy labels.}
  \item{minimum_contrast}{Minimum mean posterior-probability contrast over alternative strategies.}
}
\value{The documented eyeprocess object, data frame, plot, report path, or adapter result.}
\description{Part of the research-scale validation, advanced-model, interoperability, storage, adapter, or reproducibility programme. Experimental model functions remain subject to declared evidence gates.}
