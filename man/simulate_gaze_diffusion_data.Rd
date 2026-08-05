\name{simulate_gaze_diffusion_data}
\alias{simulate_gaze_diffusion_data}
\title{Simulate a hierarchical gaze-diffusion study}
\usage{
simulate_gaze_diffusion_data(n_person = 80L, n_item = 20L, trials_per_item = 1L,
  gaze_effect = 0.35, contaminant_fraction = 0.02, time_step = 0.002,
  max_decision_time = 10, seed = 1L)
}
\arguments{
  \item{n_person}{Number of participants/items.}
  \item{n_item}{Number of participants/items.}
  \item{trials_per_item}{Replications per person-item.}
  \item{gaze_effect}{Drift effect of gaze feature.}
  \item{contaminant_fraction}{Contaminant fraction.}
  \item{time_step}{Time step for the built-in Wiener discretization fallback.}
  \item{max_decision_time}{Maximum fallback decision time in seconds.}
  \item{seed}{Seed.}
}
\value{The documented eyeprocess object, data frame, plot, report path, or adapter result.}
\description{Part of the research-scale validation, advanced-model, interoperability, storage, adapter, or reproducibility programme. Experimental model functions remain subject to declared evidence gates.}
