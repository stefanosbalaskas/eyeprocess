\name{compare_functional_scalar_models}
\alias{compare_functional_scalar_models}
\title{Compare functional and scalar pupil summaries}
\usage{
compare_functional_scalar_models(x, scalar_features = c("pupil_peak", "pupil_auc",
  "pupil_mean"), criterion = c("AIC", "log_loss"), folds = 5L, seed = 1L)
}
\arguments{
  \item{x}{Functional pupil fit or prepared data.}
  \item{scalar_features}{Scalar feature names.}
  \item{criterion}{AIC or cross-validated log loss.}
  \item{folds}{Grouped folds for cross-validation.}
  \item{seed}{Random seed.}
}
\value{The documented eyeprocess object, data frame, plot, report path, or adapter result.}
\description{Part of the research-scale validation, advanced-model, interoperability, storage, adapter, or reproducibility programme. Experimental model functions remain subject to declared evidence gates.}
