\name{gaze-survival}
\alias{prepare_gaze_survival_data}
\alias{validate_gaze_survival_data}
\alias{summarise_gaze_censoring}
\alias{estimate_gaze_survival}
\alias{fit_gaze_cox_model}
\alias{fit_gaze_mixed_cox_model}
\alias{fit_gaze_aft_model}
\alias{tidy_gaze_survival_model}
\alias{check_gaze_proportional_hazards}
\alias{compare_gaze_survival_models}
\alias{predict_gaze_survival}
\alias{estimate_gaze_latency_quantiles}
\alias{plot_gaze_survival_curve}
\alias{plot_gaze_cumulative_incidence}
\alias{plot_gaze_hazard}
\alias{plot_gaze_cox_diagnostics}
\alias{compare_gaze_survival_specifications}
\alias{report_gaze_survival_model}
\alias{simulate_gaze_survival_inputs}
\alias{simulate_gaze_survival_example}
\title{Censored Gaze-Latency Survival Analysis}
\description{
Vendor-neutral preparation, validation, descriptive estimation, Cox and accelerated
failure-time modelling, diagnostics, sensitivity analysis, plotting, prediction, and
reporting for eye-tracking latency outcomes with explicit right censoring.
}
\usage{
prepare_gaze_survival_data(
  trials, events = NULL, target_aoi = NULL, event_type = "first_fixation",
  participant_col = "participant_id", trial_col = "trial_id",
  recording_col = "recording_id", stimulus_col = "stimulus_id",
  condition_col = "condition", trial_start_col = NULL, trial_end_col = NULL,
  time_origin_col = NULL, observation_end_reason_col = NULL,
  event_time_col = "start_time", event_aoi_col = "aoi_id",
  event_trial_col = "trial_id", event_participant_col = "participant_id",
  event_recording_col = "recording_id", episode_type_col = "episode_type",
  event_observed_col = "event_observed", supplied_event_time_col = "event_time",
  supplied_censor_time_col = "censor_time", n_valid_samples_col = "n_valid_samples",
  valid_fraction_col = "valid_data_fraction", valid_observation_col = NULL,
  min_valid_fraction = NULL, time_origin = "trial_start",
  time_unit = c("seconds", "milliseconds"), source_data = NULL,
  preprocessing_specification = NULL, event_detector = NULL,
  aoi_specification = NULL, quality_rules = list()
)
validate_gaze_survival_data(data, raise_on_error = TRUE)
summarise_gaze_censoring(data, by = NULL)
estimate_gaze_survival(data, group = NULL, conf_level = 0.95)
fit_gaze_cox_model(data, formula, ties = c("breslow", "efron", "exact"), cluster = NULL)
fit_gaze_mixed_cox_model(
  data, formula, participant_col = "participant_id",
  structure = NULL, ties = c("breslow", "efron", "exact")
)
fit_gaze_aft_model(data, formula, distribution = NULL, ...)
tidy_gaze_survival_model(model, conf_level = 0.95)
check_gaze_proportional_hazards(model, transform = "km", alpha = 0.05)
compare_gaze_survival_models(...)
predict_gaze_survival(model, newdata, times)
estimate_gaze_latency_quantiles(object, probs = c(0.25, 0.5, 0.75), group = NULL, newdata = NULL)
plot_gaze_survival_curve(data, group = NULL, ...)
plot_gaze_cumulative_incidence(data, group = NULL, ...)
plot_gaze_hazard(data, group = NULL, ...)
plot_gaze_cox_diagnostics(model, ...)
compare_gaze_survival_specifications(
  specifications, formula, model_families = c("cox_cluster_robust", "aft_weibull"),
  participant_col = "participant_id"
)
report_gaze_survival_model(model, conf_level = 0.95)
simulate_gaze_survival_inputs(kind = c("disclosure", "verification"), seed = 20260918, n_participants = 36L, trials_per_participant = 3L)
simulate_gaze_survival_example(kind = c("disclosure", "verification"), seed = 20260918, n_participants = 36L, trials_per_participant = 3L)
}
\arguments{
  \item{trials}{Trial-level observation-window table. Each participant/trial key must be unique.}
  \item{events}{Optional fixation or AOI-visit event table.}
  \item{target_aoi}{Target AOI used to define the event of interest.}
  \item{event_type}{Event definition, including first fixation, first AOI entry, first evidence inspection, first revisit, first transition, or disengagement.}
  \item{participant_col,trial_col,recording_col,stimulus_col,condition_col}{Column names defining trial identity and design fields.}
  \item{trial_start_col,trial_end_col,time_origin_col,observation_end_reason_col}{Observation-window and time-origin columns.}
  \item{event_time_col,event_aoi_col,event_trial_col,event_participant_col,event_recording_col,episode_type_col}{Event-table mappings.}
  \item{event_observed_col,supplied_event_time_col,supplied_censor_time_col}{Columns used when event/censoring values are supplied directly.}
  \item{n_valid_samples_col,valid_fraction_col,valid_observation_col,min_valid_fraction}{Quality fields and explicit minimum-validity rule.}
  \item{time_origin}{Named time origin recorded in the canonical table.}
  \item{time_unit}{Input time unit. Units are never changed silently.}
  \item{source_data,preprocessing_specification,event_detector,aoi_specification,quality_rules}{Provenance metadata retained on derived data.}
  \item{data}{Canonical gaze-survival data.}
  \item{raise_on_error}{Whether validation errors should stop execution.}
  \item{by,group}{Optional grouping variable for descriptive summaries or curves.}
  \item{conf_level}{Confidence level for intervals.}
  \item{formula}{Right-hand-side model formula or formula string.}
  \item{ties}{Cox tie-handling method.}
  \item{cluster}{Optional cluster column for robust Cox uncertainty.}
  \item{structure}{Required repeated-participant Cox structure: \code{"cluster_robust"} or \code{"frailty"}. No estimator is selected implicitly.}
  \item{distribution}{Required AFT distribution: \code{"weibull"} or \code{"lognormal"}. No AFT family is selected implicitly.}
  \item{model,object}{A fitted gaze-survival model or descriptive survival estimate.}
  \item{transform,alpha}{Proportional-hazards diagnostic settings.}
  \item{newdata,times,probs}{Prediction and latency-quantile inputs.}
  \item{specifications}{Named collection of explicitly prepared alternative survival datasets.}
  \item{model_families}{Explicit model families to fit across sensitivity branches.}
  \item{kind}{Synthetic example family: disclosure inspection or evidence verification.}
  \item{seed,n_participants,trials_per_participant}{Deterministic synthetic-example controls.}
  \item{...}{Additional arguments passed to the selected backend or plotting method.}
}
\details{
A missing target event is not automatically censoring. A row is right-censored only
when the observation window is known and the trial is otherwise analyzable. Rows
with incomplete or unusable gaze remain explicit review states. Cox models estimate
hazard ratios; AFT models estimate multiplicative time effects. Participant-clustered
Cox uncertainty and Gaussian frailty Cox models are distinct estimators and are never
silently substituted. Cox partial-likelihood information criteria are not treated as
rank-comparable with AFT full-likelihood information criteria.
}
\value{
Preparation returns a canonical trial-level data frame. Validation and summaries
return data frames. Estimation/model helpers return survival/model objects that retain
model specification and provenance. Plot helpers return their plotting objects.
Synthetic helpers return deterministic raw trial/event inputs or prepared canonical data.
}
\examples{
raw <- simulate_gaze_survival_inputs(seed = 1, n_participants = 12, trials_per_participant = 3)
surv <- prepare_gaze_survival_data(
  raw$trials, raw$events,
  target_aoi = "disclosure",
  source_data = "synthetic disclosure example",
  event_detector = "synthetic fixation events",
  aoi_specification = "disclosure rectangle"
)
summarise_gaze_censoring(surv, by = "condition")
validate_gaze_survival_data(surv, raise_on_error = FALSE)

if (requireNamespace("survival", quietly = TRUE)) {
  km <- estimate_gaze_survival(surv, group = "condition")
  cox <- fit_gaze_mixed_cox_model(
    surv, ~ condition,
    participant_col = "participant_id",
    structure = "cluster_robust"
  )
  tidy_gaze_survival_model(cox)
}
}
\seealso{
\code{\link[survival:Surv]{survival::Surv}},
\code{\link[survival:coxph]{survival::coxph}},
\code{\link[survival:survreg]{survival::survreg}}
}