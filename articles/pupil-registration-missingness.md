# Pupil Phase-Amplitude Registration and Missingness

Registration separates when a pupil response occurs from how large its
response is.

``` r

registration <- register_pupil_curves(
  pupil_long,
  time = "time_from_stimulus",
  pupil = "pupil_bc",
  id_col = "person_trial_id",
  method = "elastic"
)
plot_pupil_registration(registration)
plot_warping_functions(registration)
components <- decompose_pupil_phase_amplitude(registration)
plot_phase_amplitude_scores(components)
audit_pupil_registration(registration)
```

Observation probability must also be modelled when pupil or gaze
availability is related to effort, motion, condition, or item
difficulty.

``` r

observation <- fit_process_observation_model(
  trial_features,
  observed = "pupil_observed",
  predictors = c("item_difficulty", "condition", "blink_rate", "head_motion")
)
plot_observation_probability(observation)
selection <- fit_joint_signal_missingness(
  outcome = "pupil_auc",
  observation = observation,
  x = trial_features,
  predictors = c("item_difficulty", "condition")
)
sensitivity <- process_pattern_mixture(
  trial_features,
  metric = "pupil_auc",
  delta = seq(-1, 1, 0.1)
)
plot_mnar_tipping_point(sensitivity_mnar_process(sensitivity))
```
