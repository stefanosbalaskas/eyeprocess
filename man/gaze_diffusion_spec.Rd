\name{gaze_diffusion_spec}
\alias{gaze_diffusion_spec}
\title{Define a gaze-informed diffusion model}
\usage{
gaze_diffusion_spec(response = "score", response_time = "response_time",
  participant = "participant_id", item = "item_id", drift_features = character(),
  boundary_features = character(), nondecision_features = character(),
  starting_features = character(), censor_column = NULL, contaminant = TRUE,
  engine = c("baseline", "stan", "ez_regression", "diffIRT", "brms"),
  gaze_features = NULL, chains = 4L, parallel_chains = min(4L, chains),
  iter_warmup = 1000L, iter_sampling = 1000L, adapt_delta = 0.97, max_treedepth = 13L)
}
\arguments{
  \item{response}{Binary response column.}
  \item{response_time}{Response-time column in seconds.}
  \item{participant}{Participant identifier.}
  \item{item}{Item identifier.}
  \item{drift_features}{Features assigned a priori to drift rate.}
  \item{boundary_features}{Features assigned a priori to boundary separation.}
  \item{nondecision_features}{Features assigned a priori to non-decision time.}
  \item{starting_features}{Features assigned a priori to starting-point bias.}
  \item{censor_column}{Optional censoring column with `observed`, `left`, or `right`.}
  \item{contaminant}{Whether to estimate a uniform contaminant mixture.}
  \item{engine}{Baseline approximation or Stan Wiener model.}
  \item{gaze_features}{Value for `gaze_features`. See the function description and relevant article for constraints.}
  \item{chains}{Stan controls.}
  \item{parallel_chains}{Stan controls.}
  \item{iter_warmup}{Stan controls.}
  \item{iter_sampling}{Stan controls.}
  \item{adapt_delta}{Stan controls.}
  \item{max_treedepth}{Stan controls.}
}
\value{The documented eyeprocess object, data frame, plot, report path, or adapter result.}
\description{Part of the research-scale validation, advanced-model, interoperability, storage, adapter, or reproducibility programme. Experimental model functions remain subject to declared evidence gates.}
