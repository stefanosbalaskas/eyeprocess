\name{simulate_dynamic_irtree_data}
\alias{simulate_dynamic_irtree_data}
\title{Simulate observed dynamic-state transitions}
\usage{
simulate_dynamic_irtree_data(n_person = 100L, n_item = 20L,
  transitions_per_trial = 8L, states = c("prompt", "evidence", "options"),
  beta_response = 0.5, person_sd = 0.4, item_sd = 0.3, irregular_time = TRUE,
  state_misclassification = 0, missing_state = 0, structural_zeros = NULL, seed = 1L)
}
\arguments{
  \item{n_person}{Number of persons and items.}
  \item{n_item}{Number of persons and items.}
  \item{transitions_per_trial}{Number of transitions per person-item trial.}
  \item{states}{State labels.}
  \item{beta_response}{Response effect on transitions.}
  \item{person_sd}{Person/item heterogeneity.}
  \item{item_sd}{Person/item heterogeneity.}
  \item{irregular_time}{Whether to generate irregular time gaps.}
  \item{state_misclassification}{Destination-state error probability.}
  \item{missing_state}{Missing destination-state probability.}
  \item{structural_zeros}{Optional forbidden transitions.}
  \item{seed}{Random seed.}
}
\value{The documented eyeprocess object, data frame, plot, report path, or adapter result.}
\description{Part of the research-scale validation, advanced-model, interoperability, storage, adapter, or reproducibility programme. Experimental model functions remain subject to declared evidence gates.}
