\name{query_eye_storage}
\alias{query_eye_storage}
\title{Query partitioned eye storage lazily where possible}
\usage{
query_eye_storage(storage, table, filters = list(), columns = NULL, collect = TRUE)
}
\arguments{
  \item{storage}{Storage object or path.}
  \item{table}{Canonical table.}
  \item{filters}{Named list of equality filters.}
  \item{columns}{Optional selected columns.}
  \item{collect}{Whether to collect an Arrow query.}
}
\value{The documented eyeprocess object, data frame, plot, report path, or adapter result.}
\description{Part of the research-scale validation, advanced-model, interoperability, storage, adapter, or reproducibility programme. Experimental model functions remain subject to declared evidence gates.}
