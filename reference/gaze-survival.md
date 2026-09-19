# Censored Gaze-Latency Survival Analysis

Vendor-neutral preparation, validation, descriptive estimation, Cox and
accelerated failure-time modelling, diagnostics, sensitivity analysis,
plotting, prediction, and reporting for eye-tracking latency outcomes
with explicit right censoring.

## Usage

``` r
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
estimate_gaze_latency_quantiles(
  object, probs = c(0.25, 0.5, 0.75), group = NULL,
  newdata = NULL
)
plot_gaze_survival_curve(data, group = NULL, ...)
plot_gaze_cumulative_incidence(data, group = NULL, ...)
plot_gaze_hazard(data, group = NULL, ...)
plot_gaze_cox_diagnostics(model, ...)
compare_gaze_survival_specifications(
  specifications, formula, model_families,
  participant_col = "participant_id", ties = "breslow"
)
report_gaze_survival_model(model, conf_level = 0.95)
simulate_gaze_survival_inputs(
  kind = c("disclosure", "verification"), seed = 20260918,
  n_participants = 36L, trials_per_participant = 3L
)
simulate_gaze_survival_example(
  kind = c("disclosure", "verification"), seed = 20260918,
  n_participants = 36L, trials_per_participant = 3L
)
```

## Arguments

- trials:

  Trial-level observation-window table. Each participant/trial key must
  be unique.

- events:

  Optional fixation or AOI-visit event table.

- target_aoi:

  Target AOI used to define the event of interest.

- event_type:

  Event definition, including first fixation, first AOI entry, first
  evidence inspection, first revisit, first transition, or
  disengagement.

- participant_col,trial_col,recording_col,stimulus_col,condition_col:

  Column names defining trial identity and design fields.

- trial_start_col,trial_end_col,time_origin_col,observation_end_reason_col:

  Observation-window and time-origin columns.

- event_time_col,event_aoi_col,event_trial_col,event_participant_col,event_recording_col,episode_type_col:

  Event-table mappings.

- event_observed_col,supplied_event_time_col,supplied_censor_time_col:

  Columns used when event/censoring values are supplied directly.

- n_valid_samples_col,valid_fraction_col,valid_observation_col,min_valid_fraction:

  Quality fields and explicit minimum-validity rule.

- time_origin:

  Named time origin recorded in the canonical table.

- time_unit:

  Input time unit. Units are never changed silently.

- source_data,preprocessing_specification,event_detector,aoi_specification,quality_rules:

  Provenance metadata retained on derived data.

- data:

  Canonical gaze-survival data.

- raise_on_error:

  Whether validation errors should stop execution.

- by,group:

  Optional grouping variable for descriptive summaries or curves.

- conf_level:

  Confidence level for intervals.

- formula:

  Right-hand-side model formula or formula string.

- ties:

  Cox tie-handling method.

- cluster:

  Optional cluster column for robust Cox uncertainty.

- structure:

  Required repeated-participant Cox structure: `"cluster_robust"` or
  `"frailty"`. No estimator is selected implicitly.

- distribution:

  Required AFT distribution: `"weibull"` or `"lognormal"`. No AFT family
  is selected implicitly.

- model,object:

  A fitted gaze-survival model or descriptive survival estimate.

- transform,alpha:

  Transformation and significance threshold used by the
  proportional-hazards diagnostic.

- newdata,times,probs:

  Prediction and latency-quantile inputs.

- specifications:

  Named collection of explicitly prepared alternative survival datasets.

- model_families:

  Explicit model families to fit across sensitivity branches.

- kind:

  Synthetic example family: disclosure inspection or evidence
  verification.

- seed,n_participants,trials_per_participant:

  Deterministic synthetic-example controls.

- ...:

  Additional arguments passed to the selected backend or plotting
  method.

## Details

A missing target event is not automatically censoring. A row is
right-censored only when the observation window is known and the trial
is otherwise analyzable. Rows with incomplete or unusable gaze remain
explicit review states. Cox models estimate hazard ratios; AFT models
estimate multiplicative time effects. Participant-clustered Cox
uncertainty and Gaussian frailty Cox models are distinct estimators and
are never silently substituted. Information criteria from ordinary Cox
partial likelihood, `coxme` penalized frailty likelihood, and AFT full
likelihood are not treated as rank-comparable across likelihood bases.

## Value

Preparation returns a canonical trial-level data frame. Validation and
summaries return data frames. Estimation/model helpers return
survival/model objects that retain model specification and provenance.
Plot helpers return their plotting objects. Synthetic helpers return
deterministic raw trial/event inputs or prepared canonical data.

## See also

[`survival::Surv`](https://rdrr.io/pkg/survival/man/Surv.html),
[`survival::coxph`](https://rdrr.io/pkg/survival/man/coxph.html),
[`survival::survreg`](https://rdrr.io/pkg/survival/man/survreg.html)

## Examples

``` r
raw <- simulate_gaze_survival_inputs(seed = 1, n_participants = 12, trials_per_participant = 3)
surv <- prepare_gaze_survival_data(
  raw$trials, raw$events,
  target_aoi = "disclosure",
  source_data = "synthetic disclosure example",
  event_detector = "synthetic fixation events",
  aoi_specification = "disclosure rectangle"
)
summarise_gaze_censoring(surv, by = "condition")
#>             condition n_trials n_analyzable n_observed_events n_censored
#> 1             control       12           12                 9          3
#> 2 detailed_disclosure       12           12                11          1
#> 3  minimal_disclosure       12           12                11          1
#>   n_review_required censoring_fraction
#> 1                 0         0.25000000
#> 2                 0         0.08333333
#> 3                 0         0.08333333
validate_gaze_survival_data(surv, raise_on_error = FALSE)
#> [1] severity code     n        message 
#> <0 rows> (or 0-length row.names)

if (requireNamespace("survival", quietly = TRUE)) {
  km <- estimate_gaze_survival(surv, group = "condition")
  cox <- fit_gaze_mixed_cox_model(
    surv, ~ condition,
    participant_col = "participant_id",
    structure = "cluster_robust"
  )
  tidy_gaze_survival_model(cox)
}
#>                           term estimate_log_scale std_error hazard_ratio
#> 1 conditiondetailed_disclosure          0.9003854 0.5002911     2.460551
#> 2  conditionminimal_disclosure          0.7076579 0.4448383     2.029233
#>    conf_low conf_high statistic    p_value effect_measure
#> 1 0.9229620  6.559655  1.799723 0.07190439   hazard_ratio
#> 2 0.8485645  4.852650  1.590821 0.11164997   hazard_ratio
```
