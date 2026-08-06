\name{prepare_dynamic_irtree_data}
\alias{prepare_dynamic_irtree_data}
\title{Prepare ordered transition data for dynamic IRTree models}
\usage{
prepare_dynamic_irtree_data(x, spec = dynamic_irtree_spec(),
  person = "participant_id", item = "item_id", trial = "trial_id", state = "state",
  time = NULL, from = "from_state", to = "to_state", states = NULL)
}
\arguments{
  \item{x}{An `eye_dataset` or data frame.}
  \item{spec}{Dynamic IRTree specification.}
  \item{person}{Column names for long state data.}
  \item{item}{Column names for long state data.}
  \item{trial}{Column names for long state data.}
  \item{state}{Column names for long state data.}
  \item{time}{Column names for long state data.}
  \item{from}{Existing transition columns for transition-format data.}
  \item{to}{Existing transition columns for transition-format data.}
  \item{states}{Optional complete state vocabulary.}
}
\value{The documented eyeprocess object, data frame, plot, report path, or adapter result.}
\description{Part of the research-scale validation, advanced-model, interoperability, storage, adapter, or reproducibility programme. Experimental model functions remain subject to declared evidence gates.}
