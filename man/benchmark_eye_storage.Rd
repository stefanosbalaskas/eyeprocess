\name{benchmark_eye_storage}
\alias{benchmark_eye_storage}
\title{Benchmark storage formats and query operations}
\usage{
benchmark_eye_storage(x, formats = c("rds", "csv", "parquet"),
  partition_by = c("participant_id", "recording_id"), repetitions = 3L,
  directory = tempdir())
}
\arguments{
  \item{x}{Named list of tables or eye dataset.}
  \item{formats}{Formats to benchmark.}
  \item{partition_by}{Partition columns.}
  \item{repetitions}{Repetitions.}
  \item{directory}{Parent temporary directory.}
}
\value{The documented eyeprocess object, data frame, plot, report path, or adapter result.}
\description{Part of the research-scale validation, advanced-model, interoperability, storage, adapter, or reproducibility programme. Experimental model functions remain subject to declared evidence gates.}
