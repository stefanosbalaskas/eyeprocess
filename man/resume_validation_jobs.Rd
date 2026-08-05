\name{resume_validation_jobs}
\alias{resume_validation_jobs}
\title{Resume incomplete or failed validation jobs}
\usage{
resume_validation_jobs(plan, output_dir, retry = c("missing", "failed",
  "nonconverged", "locked", "corrupt"), ...)
}
\arguments{
  \item{plan}{Validation plan or manifest path.}
  \item{output_dir}{Validation output directory.}
  \item{retry}{Which statuses to re-run.}
  \item{...}{Passed to `run_validation_jobs()`.}
}
\value{The documented eyeprocess object, data frame, plot, report path, or adapter result.}
\description{Part of the research-scale validation, advanced-model, interoperability, storage, adapter, or reproducibility programme. Experimental model functions remain subject to declared evidence gates.}
