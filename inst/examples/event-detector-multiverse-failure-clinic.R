# Synthetic, deterministic failure clinic for detector sensitivity.
d <- simulate_detector_multiverse_data(
  n_participants = 4,
  seed = 20260919
)

ivt30 <- define_event_detector_spec(
  "ivt30",
  "ivt",
  velocity_threshold = 30,
  minimum_duration_ms = 60,
  maximum_gap_ms = 75,
  sampling_rate = 60,
  coordinate_unit = "degrees"
)

external_fail <- define_event_detector_spec(
  "external_fail",
  "external",
  implementation = "deliberate_failure_fixture",
  callback = function(data, spec) {
    stop("deliberate external detector failure")
  }
)

detected <- run_detector_multiverse(
  d,
  list(ivt30, external_fail),
  continue_on_error = TRUE
)
stopifnot(setequal(detected$status$status, c("ok", "failed")))
stopifnot(any(grepl(
  "deliberate external detector failure",
  detected$failures$error,
  fixed = TRUE
)))

features <- propagate_detector_to_aoi(detected)
features <- propagate_detector_to_features(features)
idx <- which(
  features$features$detector_id == "ivt30" &
    as.character(features$features$aoi_id) == "disclosure"
)
stopifnot(length(idx) >= 3L)

# Normalize synthetic target rows so the next two attrition events are deliberate.
features$features$valid_data_fraction[idx] <- 1
bad_dwell <- !is.finite(as.numeric(features$features$dwell_time_ms[idx]))
if (any(bad_dwell)) {
  features$features$dwell_time_ms[idx[bad_dwell]] <- 100
}
features$features$valid_data_fraction[idx[1]] <- .1
features$features$dwell_time_ms[idx[2]] <- NA_real_

tidy_callback <- function(data, model_spec) {
  data.frame(
    term = "condition",
    estimate = 1,
    SE = .2,
    CI_lower = .6,
    CI_upper = 1.4,
    p = .01,
    converged = TRUE,
    N = nrow(data),
    stringsAsFactors = FALSE
  )
}

inference <- run_detector_inference_multiverse(
  features,
  list(
    engine = "callback",
    formula = dwell_time_ms ~ condition_id,
    outcome = "dwell_time_ms",
    aoi_id = "disclosure"
  ),
  model_callback = tidy_callback,
  minimum_valid_fraction = .5
)
audit <- inference$input_audit[
  inference$input_audit$detector_id == "ivt30",
  ,
  drop = FALSE
]
stopifnot(audit$quality_excluded_rows == 1L)
stopifnot(audit$outcome_missing_rows == 1L)
stopifnot(audit$status == "modelled")

duplicate_callback <- function(data, model_spec) {
  row <- tidy_callback(data, model_spec)
  rbind(row, row)
}
invalid <- run_detector_inference_multiverse(
  features,
  list(
    engine = "callback",
    formula = dwell_time_ms ~ condition_id,
    outcome = "dwell_time_ms",
    aoi_id = "disclosure"
  ),
  model_callback = duplicate_callback
)
invalid_failure <- invalid$failures[
  invalid$failures$detector_id == "ivt30",
  ,
  drop = FALSE
]
stopifnot(any(grepl(
  "at most one row per coefficient term",
  invalid_failure$error,
  fixed = TRUE
)))

print(detected$status)
print(detected$failures)
print(inference$input_audit)
print(inference$warnings)
print(invalid$failures)
