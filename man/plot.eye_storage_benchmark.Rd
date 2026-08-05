\name{plot.eye_storage_benchmark}
\alias{plot.eye_storage_benchmark}
\title{plot eye storage benchmark}
\usage{
\method{plot}{eye_storage_benchmark}(x, metric = c("write_seconds", "read_seconds",
  "bytes"), ...)
}
\arguments{
  \item{x}{Value for `x`. See the function description and relevant article for constraints.}
  \item{metric}{Value for `metric`. See the function description and relevant article for constraints.}
  \item{...}{Additional arguments passed to the selected engine or method.}
}
\value{The documented eyeprocess object, data frame, plot, report path, or adapter result.}
\description{S3 method supporting a research-scale eyeprocess object.}
