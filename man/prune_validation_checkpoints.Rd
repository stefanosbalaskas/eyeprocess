\name{prune_validation_checkpoints}
\alias{prune_validation_checkpoints}
\title{Remove obsolete or corrupt validation checkpoints}
\usage{
prune_validation_checkpoints(path, statuses = c("corrupt", "locked"), dry_run = TRUE)
}
\arguments{
  \item{path}{Validation output directory.}
  \item{statuses}{Statuses to remove.}
  \item{dry_run}{Report without deleting.}
}
\value{The documented eyeprocess object, data frame, plot, report path, or adapter result.}
\description{Part of the research-scale validation, advanced-model, interoperability, storage, adapter, or reproducibility programme. Experimental model functions remain subject to declared evidence gates.}
