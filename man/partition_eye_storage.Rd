\name{partition_eye_storage}
\alias{partition_eye_storage}
\title{Create a partition specification}
\usage{
partition_eye_storage(by = c("participant_id", "session_id", "recording_id"),
  format = c("parquet", "csv", "rds"), compression = "zstd", max_rows = 1000000L)
}
\arguments{
  \item{by}{Partition columns.}
  \item{format}{Storage format.}
  \item{compression}{Compression codec for Parquet.}
  \item{max_rows}{Maximum rows per physical file.}
}
\value{The documented eyeprocess object, data frame, plot, report path, or adapter result.}
\description{Part of the research-scale validation, advanced-model, interoperability, storage, adapter, or reproducibility programme. Experimental model functions remain subject to declared evidence gates.}
