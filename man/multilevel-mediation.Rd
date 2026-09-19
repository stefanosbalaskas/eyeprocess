\name{multilevel-mediation}
\alias{multilevel-mediation}
\alias{center_within_participant}
\alias{decompose_within_between}
\alias{summarise_within_between_variance}
\alias{identify_mediation_levels}
\alias{audit_mediation_missingness}
\alias{check_mediation_trial_counts}
\alias{validate_multilevel_mediation_data}
\alias{prepare_multilevel_mediation_data}
\alias{add_multilevel_mediation_component}
\title{Trial-level multilevel mediation preparation}
\description{
Vendor-neutral preparation and auditing utilities for repeated-measures mediation.
The functions preserve trial-level rows, separate within- and between-participant
variation, distinguish observed zero mediators from unobserved mediators, and
retain provenance needed by downstream specialist estimators.
}
\usage{
center_within_participant(
  data, value_col, participant_col = "participant_id", output_col = NULL
)

decompose_within_between(
  data, columns, participant_col = "participant_id",
  grand_mean_center_between = FALSE
)

summarise_within_between_variance(
  data, columns, participant_col = "participant_id"
)

identify_mediation_levels(
  data, columns, participant_col = "participant_id", tolerance = 1e-12
)

audit_mediation_missingness(
  data, x_col, mediator_col, outcome_col,
  participant_col = "participant_id", trial_col = "trial_id",
  quality_col = NULL, minimum_quality = NULL,
  mediator_observed_col = NULL, response_observed_col = NULL
)

check_mediation_trial_counts(
  data, participant_col = "participant_id", trial_col = "trial_id",
  minimum_trials = 2L
)

validate_multilevel_mediation_data(
  data, x_col, mediator_col, outcome_col,
  participant_col = "participant_id", trial_col = "trial_id",
  require_within_x = TRUE
)

prepare_multilevel_mediation_data(
  data, x_col, mediator_col, outcome_col,
  participant_col = "participant_id", trial_col = "trial_id",
  quality_col = NULL, minimum_quality = NULL,
  mediator_observed_col = NULL, response_observed_col = NULL,
  quality_action = c("flag", "mask_mediator"),
  grand_mean_center_between = FALSE, require_within_x = TRUE,
  source_id = NULL, preprocessing_spec = NULL, event_detector = NULL,
  aoi_specification = NULL, warn = TRUE
)

add_multilevel_mediation_component(
  prepared, value_col, semantic, within_col, between_col,
  grand_mean_center_between = NULL
)
}
\arguments{
  \item{data}{Repeated-measures trial-level data frame. Rows are preserved.}
  \item{value_col}{Numeric variable to decompose or add as a mediation component.}
  \item{columns}{One or more numeric variables to inspect or decompose.}
  \item{participant_col}{Participant identifier column.}
  \item{trial_col}{Trial identifier column.}
  \item{output_col}{Optional name for a within-participant centered variable.}
  \item{grand_mean_center_between}{Whether participant means are centered around the observed grand mean.}
  \item{tolerance}{Non-negative tolerance for classifying within/between variation.}
  \item{x_col}{Exposure or manipulation column.}
  \item{mediator_col}{Trial-level mediator column.}
  \item{outcome_col}{Trial-level outcome column.}
  \item{quality_col}{Optional numeric trial-quality column.}
  \item{minimum_quality}{Required finite threshold when \code{quality_col} is supplied.}
  \item{mediator_observed_col}{Optional explicit marker that the mediator was observed.}
  \item{response_observed_col}{Optional explicit marker that the outcome was observed.}
  \item{minimum_trials}{Minimum trial count used for the descriptive participant audit.}
  \item{require_within_x}{Whether preparation requires within-participant exposure variation.}
  \item{quality_action}{Either \code{"flag"} or \code{"mask_mediator"}; no quality action is selected silently.}
  \item{source_id}{Optional source-data identifier retained in provenance.}
  \item{preprocessing_spec,event_detector,aoi_specification}{Optional preprocessing, event-detector, and AOI provenance.}
  \item{warn}{Whether preparation emits informative warnings stored in the returned object.}
  \item{prepared}{An \code{eye_multilevel_mediation_data} object.}
  \item{semantic}{Semantic name used to register an additional mediation component.}
  \item{within_col,between_col}{Explicit output columns for an added within/between component.}
}
\details{
The preparation layer does not fit a mediation estimator and does not convert
missing mediators to zero. Quality rules are explicit. When
\code{quality_action = "mask_mediator"}, masking occurs before decomposition and
is recorded in provenance. Within- and between-participant components are kept
separate so a downstream multilevel estimator can model the intended level
rather than conflating trial and person variation.
}
\value{
Centering and decomposition functions return data frames. Audit functions return
data frames or validation objects. \code{prepare_multilevel_mediation_data()}
returns an \code{eye_multilevel_mediation_data} object containing the preserved
trial table, audits, column mappings, and provenance.
}
\examples{
d <- data.frame(
  participant_id = rep(c("P1", "P2"), each = 3),
  trial_id = paste0("T", 1:6),
  condition = c(0, 1, 0, 1, 0, 1),
  dwell = c(1.0, 1.4, 1.2, 0.8, 1.1, 1.3),
  outcome = c(3, 4, 3, 2, 3, 4)
)

x <- prepare_multilevel_mediation_data(
  d,
  x_col = "condition",
  mediator_col = "dwell",
  outcome_col = "outcome"
)

head(x$data[c("X_within", "X_between", "M_within", "M_between")])
}
