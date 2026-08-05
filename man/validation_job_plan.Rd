\name{validation_job_plan}
\alias{validation_job_plan}
\title{Create a deterministic validation job plan}
\usage{
validation_job_plan(grid = NULL, replications = 100L, base_seed = 1L,
  model_family = "unspecified", plan_id = NULL, chunk_size = 1L, metadata = list())
}
\arguments{
  \item{grid}{Scenario grid or named list of factor levels.}
  \item{replications}{Replications per design cell.}
  \item{base_seed}{Base seed used for deterministic seed allocation.}
  \item{model_family}{Model-family label.}
  \item{plan_id}{Optional explicit plan identifier.}
  \item{chunk_size}{Number of jobs assigned to each chunk.}
  \item{metadata}{Arbitrary plan metadata.}
}
\value{The documented eyeprocess object, data frame, plot, report path, or adapter result.}
\description{Part of the research-scale validation, advanced-model, interoperability, storage, adapter, or reproducibility programme. Experimental model functions remain subject to declared evidence gates.}
