# Synthetic, reproducible evidence-verification survival workflow.
# The example begins with raw trial windows plus AOI-visit events.
if (requireNamespace("survival", quietly = TRUE)) {
  raw <- simulate_gaze_survival_inputs(
    "verification",
    seed = 20260918,
    n_participants = 36,
    trials_per_participant = 3
  )

  verification <- prepare_gaze_survival_data(
    raw$trials,
    raw$events,
    target_aoi = "source_evidence",
    event_type = "first_aoi_entry",
    condition_col = "condition_id",
    observation_end_reason_col = "observation_end_reason",
    min_valid_fraction = .90,
    source_data = "synthetic_verification_trial_event_inputs",
    preprocessing_specification = "synthetic_truth_no_filtering",
    event_detector = "synthetic_truth",
    aoi_specification = "fixed synthetic source/evidence AOI",
    quality_rules = list(valid_fraction_min = .90)
  )

  print(validate_gaze_survival_data(verification, raise_on_error = FALSE))
  print(summarise_gaze_censoring(verification, by = "condition"))

  cox <- fit_gaze_mixed_cox_model(
    verification,
    "condition",
    participant_col = "participant_id",
    structure = "cluster_robust"
  )
  weibull <- fit_gaze_aft_model(
    verification,
    "condition",
    distribution = "weibull"
  )
  lognormal <- fit_gaze_aft_model(
    verification,
    "condition",
    distribution = "lognormal"
  )

  print(tidy_gaze_survival_model(cox))
  print(check_gaze_proportional_hazards(cox))
  print(compare_gaze_survival_models(cox, weibull, lognormal))
  print(report_gaze_survival_model(cox))

  plot_gaze_survival_curve(verification, group = "condition")
}
