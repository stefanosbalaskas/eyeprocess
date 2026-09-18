# Event-detector multiverse worked example
#
# Synthetic, deterministic, CI-sized, and free of private participant data.

library(eyeprocess)

data <- simulate_detector_multiverse_data(
  n_participants = 6,
  sampling_rate = 60,
  seed = 20260918
)

specs <- list(
  define_event_detector_spec(
    "ivt25", "ivt",
    velocity_threshold = 25,
    minimum_duration_ms = 60,
    maximum_gap_ms = 75,
    sampling_rate = 60
  ),
  define_event_detector_spec(
    "ivt30", "ivt",
    velocity_threshold = 30,
    minimum_duration_ms = 60,
    maximum_gap_ms = 75,
    sampling_rate = 60
  ),
  define_event_detector_spec(
    "ivt35", "ivt",
    velocity_threshold = 35,
    minimum_duration_ms = 60,
    maximum_gap_ms = 75,
    sampling_rate = 60
  ),
  define_event_detector_spec(
    "idt_A", "idt",
    dispersion_threshold = 1.2,
    minimum_duration_ms = 80,
    sampling_rate = 60
  ),
  define_event_detector_spec(
    "adaptive", "adaptive_velocity",
    minimum_duration_ms = 60,
    maximum_gap_ms = 75,
    sampling_rate = 60,
    parameters = list(
      noise_factor = 4,
      minimum_velocity_threshold = 20
    )
  )
)

multiverse <- create_detector_multiverse(specs)
result <- run_detector_multiverse(data, multiverse)
result <- propagate_detector_to_aoi(result, overlap = "error")
result <- propagate_detector_to_features(result)

model_spec <- list(
  engine = "stats_lm",
  formula = dwell_time_ms ~ condition_id + participant_id,
  outcome = "dwell_time_ms",
  aoi_id = "disclosure"
)

inference <- run_detector_inference_multiverse(
  result,
  model_spec,
  minimum_valid_fraction = 0.5
)

condition_term <- grep(
  "condition_id",
  inference$coefficients$term,
  value = TRUE
)[1]

robustness <- summarise_detector_robustness(
  result,
  inference = inference,
  term = condition_term,
  substantive_threshold = 100
)

print(result$status)
print(robustness$event_summary)
print(robustness$feature_sensitivity)
print(robustness$inference_stability)

# Optional mature external detector:
#
# remodnav <- define_event_detector_spec(
#   "remodnav", "remodnav",
#   minimum_duration_ms = 60,
#   sampling_rate = 60,
#   parameters = list(noise_factor = 5)
# )
#
# If the REMoDNaV executable is unavailable, that branch is recorded as failed.
# eyeprocess never silently substitutes a different detector.
