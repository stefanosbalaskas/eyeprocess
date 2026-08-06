\name{dynamic_irtree_spec}
\alias{dynamic_irtree_spec}
\title{Specify a hardened dynamic gaze-state IRTree}
\usage{
dynamic_irtree_spec(source = c("samples", "visits", "fixations"),
  collapse_consecutive = TRUE, engine = c("baseline", "multinomial", "stan"),
  hidden_states = 0L, include_response = TRUE, include_person = FALSE,
  include_item = TRUE, condition_columns = character(),
  transition_predictors = character(), interactions = character(),
  include_time_gap = TRUE, person_effect = c("none", "fixed", "random"),
  item_effect = c("none", "fixed", "random"), structural_zeros = NULL,
  allowed_transitions = NULL, hidden_structural_zeros = NULL,
  hidden_allowed_transitions = NULL, missing_state = c("drop", "unknown",
  "marginalize"), uncertain_state_probability = NULL, misclassification_matrix = NULL,
  ridge = 1e-4, standardize = TRUE, reference_state = NULL, chains = 4L,
  parallel_chains = chains, iter_warmup = 1000L, iter_sampling = 1000L,
  adapt_delta = 0.95, max_treedepth = 12L)
}
\arguments{
  \item{source}{Sequence source for an `eye_dataset`.}
  \item{collapse_consecutive}{Collapse consecutive identical states.}
  \item{engine}{Estimation engine: auditable binary baseline, penalized}
  \item{hidden_states}{Number of latent states. Zero fits observed-state}
  \item{include_response}{Include item response as a predictor.}
  \item{include_person}{Include person effects.}
  \item{include_item}{Include item effects.}
  \item{condition_columns}{Condition-level predictors.}
  \item{transition_predictors}{Additional transition-level predictors.}
  \item{interactions}{Optional interaction terms supplied as formula strings.}
  \item{include_time_gap}{Include log time gap for irregular observations.}
  \item{person_effect}{Person effect type for Stan.}
  \item{item_effect}{Item effect type for Stan.}
  \item{structural_zeros}{Optional forbidden observed-state transition pairs.}
  \item{allowed_transitions}{Optional allowed observed-state transition pairs.}
  \item{hidden_structural_zeros}{Optional forbidden hidden-state pairs named `state1`, `state2`, and so forth.}
  \item{hidden_allowed_transitions}{Optional explicitly allowed hidden-state pairs.}
  \item{missing_state}{Treatment of missing observed states.}
  \item{uncertain_state_probability}{Optional column containing probability of}
  \item{misclassification_matrix}{Optional observed-state misclassification matrix.}
  \item{ridge}{Penalization for the multinomial baseline.}
  \item{standardize}{Standardize numeric design columns.}
  \item{reference_state}{Optional reference destination state.}
  \item{chains}{CmdStan controls.}
  \item{parallel_chains}{CmdStan controls.}
  \item{iter_warmup}{CmdStan controls.}
  \item{iter_sampling}{CmdStan controls.}
  \item{adapt_delta}{CmdStan sampler controls.}
  \item{max_treedepth}{CmdStan sampler controls.}
}
\value{The documented eyeprocess object, data frame, plot, report path, or adapter result.}
\description{Part of the research-scale validation, advanced-model, interoperability, storage, adapter, or reproducibility programme. Experimental model functions remain subject to declared evidence gates.}
