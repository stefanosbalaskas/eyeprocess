# Worked censored gaze-latency analysis using fully synthetic data.
library(eyeprocess)

raw <- simulate_gaze_survival_inputs(
  "disclosure",
  seed = 20260918,
  n_participants = 36,
  trials_per_participant = 3
)

surv_data <- prepare_gaze_survival_data(
  raw$trials,
  raw$events,
  target_aoi = "disclosure",
  event_type = "first_fixation",
  condition_col = "condition_id",
  observation_end_reason_col = "observation_end_reason",
  min_valid_fraction = .90,
  source_data = "synthetic_disclosure_trial_event_inputs",
  preprocessing_specification = "synthetic_truth_no_filtering",
  event_detector = "synthetic_truth",
  aoi_specification = "fixed synthetic disclosure AOI",
  quality_rules = list(minimum_fixation_ms = 80, valid_fraction_min = .90)
)

print(validate_gaze_survival_data(surv_data, raise_on_error = FALSE))
print(summarise_gaze_censoring(surv_data, by = "condition"))
plot_gaze_survival_curve(surv_data, group = "condition")

cox_clustered <- fit_gaze_mixed_cox_model(
  surv_data,
  "condition",
  participant_col = "participant_id",
  structure = "cluster_robust"
)
weibull <- fit_gaze_aft_model(surv_data, "condition", distribution = "weibull")
lognormal <- fit_gaze_aft_model(surv_data, "condition", distribution = "lognormal")

print(tidy_gaze_survival_model(cox_clustered))
print(tidy_gaze_survival_model(weibull))
print(check_gaze_proportional_hazards(cox_clustered))
print(suppressWarnings(compare_gaze_survival_models(cox_clustered, weibull, lognormal)))
print(report_gaze_survival_model(cox_clustered))

# True participant frailty is a distinct estimator and uses coxme when installed.
if (requireNamespace("coxme", quietly = TRUE)) {
  frailty <- fit_gaze_mixed_cox_model(
    surv_data,
    "condition",
    participant_col = "participant_id",
    structure = "frailty"
  )
  print(tidy_gaze_survival_model(frailty))
}

# Sensitivity branches are explicit; the example relabels an AOI branch only to
# demonstrate the output contract. Real analyses should reconstruct this branch
# from the alternative AOI geometry or detector.
expanded <- surv_data
expanded$aoi_specification <- "synthetic expanded disclosure AOI"
sensitivity <- compare_gaze_survival_specifications(
  list(primary = surv_data, expanded_aoi_demo = expanded),
  "condition",
  model_families = c("cox_cluster_robust", "aft_weibull")
)
print(sensitivity)