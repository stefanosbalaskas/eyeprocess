\name{write_partitioned_eye_storage}
\alias{write_partitioned_eye_storage}
\title{Write an eye dataset as atomic partitioned storage}
\usage{
write_partitioned_eye_storage(x, path,
  spec = partition_eye_storage(format = if (requireNamespace("arrow",
  quietly = TRUE)) "parquet" else "rds"), overwrite = FALSE, tables = NULL)
}
\arguments{
  \item{x}{Eye dataset or named list of data frames.}
  \item{path}{Destination directory.}
  \item{spec}{Partition specification.}
  \item{overwrite}{Whether to replace an existing store.}
  \item{tables}{Tables to write.}
}
\value{The documented eyeprocess object, data frame, plot, report path, or adapter result.}
\description{Part of the research-scale validation, advanced-model, interoperability, storage, adapter, or reproducibility programme. Experimental model functions remain subject to declared evidence gates.}
