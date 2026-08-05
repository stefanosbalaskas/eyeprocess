\name{audit_validation_completion}
\alias{audit_validation_completion}
\title{Audit whether a validation programme is complete}
\usage{
audit_validation_completion(x, thresholds = validation_thresholds(),
  empirical_reproduction = NULL)
}
\arguments{
  \item{x}{Validation collection.}
  \item{thresholds}{Validation thresholds.}
  \item{empirical_reproduction}{Optional empirical-reproduction evidence required when configured in `thresholds`.}
}
\value{The documented eyeprocess object, data frame, plot, report path, or adapter result.}
\description{Part of the research-scale validation, advanced-model, interoperability, storage, adapter, or reproducibility programme. Experimental model functions remain subject to declared evidence gates.}
