\name{pupil_preprocessing_sensitivity}
\alias{pupil_preprocessing_sensitivity}
\title{Run functional pupil preprocessing sensitivity analysis}
\usage{
pupil_preprocessing_sensitivity(x, grid = pupil_preprocessing_grid(),
  base_spec = functional_pupil_irt_spec(engine = "two_stage_glm"), fit = TRUE,
  extractor = extract_functional_pupil_parameters, continue_on_error = TRUE, ...)
}
\arguments{
  \item{x}{Eye dataset or long pupil data.}
  \item{grid}{Sensitivity grid.}
  \item{base_spec}{Base functional pupil specification.}
  \item{fit}{Whether to fit each specification.}
  \item{extractor}{Optional result extractor.}
  \item{continue_on_error}{Record errors rather than stopping.}
  \item{...}{Passed to model fitting.}
}
\value{The documented eyeprocess object, data frame, plot, report path, or adapter result.}
\description{Part of the research-scale validation, advanced-model, interoperability, storage, adapter, or reproducibility programme. Experimental model functions remain subject to declared evidence gates.}
