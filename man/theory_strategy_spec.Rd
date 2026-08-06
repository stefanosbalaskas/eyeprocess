\name{theory_strategy_spec}
\alias{theory_strategy_spec}
\title{Define a theory-constrained strategy-mixture model}
\usage{
theory_strategy_spec(strategies = NULL, feature_columns = NULL, response = "score",
  participant = "participant_id", item = "item_id", condition = NULL,
  item_availability = NULL, engine = c("em", "stan"), multiple_starts = 10L,
  anchor_strength = 3, chains = 4L, parallel_chains = min(4L, chains),
  iter_warmup = 1000L, iter_sampling = 1000L, adapt_delta = 0.95, max_treedepth = 12L,
  prototypes = NULL, feature_sd = NULL, prior = NULL)
}
\arguments{
  \item{strategies}{Named list of strategy signatures. Each signature is a named}
  \item{feature_columns}{Process-feature columns.}
  \item{response}{Binary response column.}
  \item{participant}{Participant identifier.}
  \item{item}{Item identifier.}
  \item{condition}{Optional condition column.}
  \item{item_availability}{Optional item-by-strategy availability matrix or data frame.}
  \item{engine}{Estimation engine.}
  \item{multiple_starts}{Number of starts for the EM baseline.}
  \item{anchor_strength}{Prior/penalty strength anchoring classes to signatures.}
  \item{chains}{Stan controls.}
  \item{parallel_chains}{Stan controls.}
  \item{iter_warmup}{Stan controls.}
  \item{iter_sampling}{Stan controls.}
  \item{adapt_delta}{Stan controls.}
  \item{max_treedepth}{Stan controls.}
  \item{prototypes}{Value for `prototypes`. See the function description and relevant article for constraints.}
  \item{feature_sd}{Value for `feature_sd`. See the function description and relevant article for constraints.}
  \item{prior}{Value for `prior`. See the function description and relevant article for constraints.}
}
\value{The documented eyeprocess object, data frame, plot, report path, or adapter result.}
\description{Part of the research-scale validation, advanced-model, interoperability, storage, adapter, or reproducibility programme. Experimental model functions remain subject to declared evidence gates.}
