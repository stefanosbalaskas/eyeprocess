test_that("never-inspected valid trials are retained as right-censored", {
  trials <- data.frame(
    participant_id = c("P1", "P1", "P2", "P2"),
    trial_id = c("T1", "T2", "T1", "T2"),
    stimulus_id = c("S1", "S2", "S1", "S2"),
    condition = c("control", "detail", "control", "detail"),
    start_time = 0,
    end_time = 5,
    n_valid_samples = c(300, 298, 301, 297),
    valid_data_fraction = c(.98, .97, .99, .96)
  )
  events <- data.frame(
    participant_id = c("P1", "P1", "P1", "P2", "P2"),
    trial_id = c("T1", "T1", "T2", "T1", "T2"),
    start_time = c(1.2, 2, 1, .8, 2.7),
    aoi_id = c("disclosure", "body", "body", "disclosure", "body"),
    episode_type = "fixation"
  )
  expect_warning(
    d <- prepare_gaze_survival_data(trials, events, target_aoi = "disclosure"),
    "Fewer than five"
  )
  expect_equal(nrow(d), 4L)
  expect_equal(d$event_observed, c(1, 0, 1, 0))
  expect_true(all(d$analysis_time[d$event_observed == 0] == 5))
})

test_that("canonical recording/trial event joins are supported", {
  inputs <- simulate_gaze_survival_inputs("disclosure", seed = 99, n_participants = 4, trials_per_participant = 2)
  expect_false("participant_id" %in% names(inputs$events))
  d <- suppressWarnings(prepare_gaze_survival_data(
    inputs$trials,
    inputs$events,
    target_aoi = "disclosure",
    event_type = "first_fixation",
    condition_col = "condition_id"
  ))
  expect_equal(unique(d$event_join_key), "recording_trial")
  expect_equal(nrow(d), 8L)
})

test_that("ambiguous trial-only event joins fail", {
  trials <- data.frame(
    participant_id = c("P1", "P2"), trial_id = c("T1", "T1"),
    start_time = 0, end_time = 5
  )
  events <- data.frame(
    trial_id = "T1", start_time = 1, aoi_id = "x", episode_type = "fixation"
  )
  expect_error(
    prepare_gaze_survival_data(trials, events, target_aoi = "x"),
    "trial-only matching would be ambiguous"
  )
})

test_that("custom time origins change latency and risk windows explicitly", {
  trials <- data.frame(
    participant_id = "P1", trial_id = "T1", start_time = 0,
    stimulus_onset = 2, end_time = 10, valid_data_fraction = .99
  )
  events <- data.frame(
    participant_id = "P1", trial_id = "T1", start_time = 5,
    aoi_id = "target", episode_type = "fixation"
  )
  expect_warning(
    d <- prepare_gaze_survival_data(
      trials, events, target_aoi = "target",
      time_origin = "stimulus_onset", time_origin_col = "stimulus_onset"
    ),
    "Fewer than five"
  )
  expect_equal(d$event_time, 3)
  expect_equal(d$censor_time, 8)
  expect_equal(d$trial_duration, 10)
})

test_that("revisit, transition, and disengagement use visit-level semantics", {
  trials <- data.frame(participant_id = "P1", trial_id = "T1", start_time = 0, end_time = 5)
  events <- data.frame(
    participant_id = rep("P1", 5), trial_id = rep("T1", 5),
    start_time = c(.4, .8, 1.0, 1.2, 2.2),
    aoi_id = c("body", "target", "target", "body", "target"),
    episode_type = "fixation"
  )
  revisit <- suppressWarnings(prepare_gaze_survival_data(trials, events, target_aoi = "target", event_type = "first_revisit"))
  transition <- suppressWarnings(prepare_gaze_survival_data(trials, events, target_aoi = "target", event_type = "first_transition_into_target"))
  disengage <- suppressWarnings(prepare_gaze_survival_data(trials, events, target_aoi = "target", event_type = "disengagement"))
  expect_equal(revisit$event_time, 2.2)
  expect_equal(transition$event_time, .8)
  expect_equal(disengage$event_time, 1.2)
})

test_that("missing event time is not silently turned into censoring", {
  trials <- data.frame(participant_id = "P1", trial_id = "T1", start_time = 0, end_time = 5)
  expect_error(
    prepare_gaze_survival_data(trials, target_aoi = "x"),
    "event_observed.*supplied explicitly"
  )
})

test_that("duplicated trials and malformed times fail", {
  trials <- data.frame(
    participant_id = c("P1", "P1"), trial_id = c("T1", "T1"),
    start_time = 0, end_time = 5, event_observed = c(0, 0)
  )
  expect_error(prepare_gaze_survival_data(trials), "Duplicated")
  d <- simulate_gaze_survival_example(n_participants = 4)
  d$event_observed[1] <- 1
  d$event_time[1] <- 8
  d$analysis_time[1] <- 8
  issues <- validate_gaze_survival_data(d, raise_on_error = FALSE)
  expect_true("event_after_censor" %in% issues$code)
})

test_that("unusable gaze is review state, not censoring", {
  trials <- data.frame(
    participant_id = c("P1", "P1"), trial_id = c("T1", "T2"),
    start_time = 0, end_time = 5, valid_data_fraction = c(.98, .2)
  )
  events <- data.frame(
    participant_id = "P1", trial_id = "T1", start_time = 1,
    aoi_id = "x", episode_type = "fixation"
  )
  expect_warning(
    d <- prepare_gaze_survival_data(trials, events, target_aoi = "x", min_valid_fraction = .8),
    "Fewer than five|unknown event status"
  )
  expect_true(is.na(d$event_observed[2]))
  expect_equal(d$censor_reason[2], "unusable_gaze_quality")
})

test_that("all-event and high-censoring data retain their event contract", {
  d <- simulate_gaze_survival_example(n_participants = 10, trials_per_participant = 2)
  all_event <- d
  all_event$event_observed <- 1
  all_event$event_time <- seq(.2, 3.8, length.out = nrow(all_event))
  all_event$analysis_time <- all_event$event_time
  expect_equal(summarise_gaze_censoring(all_event)$n_censored, 0)

  high <- d
  high$event_observed <- 0
  high$event_time <- NA_real_
  high$analysis_time <- high$censor_time
  issues <- validate_gaze_survival_data(high, raise_on_error = FALSE)
  expect_true("extreme_censoring" %in% issues$code)
})

test_that("KM, Cox, clustered Cox, and AFT models run on deterministic data", {
  skip_if_not_installed("survival")
  d <- simulate_gaze_survival_example(n_participants = 40, trials_per_participant = 3)
  km <- estimate_gaze_survival(d, group = "condition")
  expect_true(nrow(km) > 0)
  cox <- fit_gaze_cox_model(d, "condition")
  clustered <- fit_gaze_mixed_cox_model(d, "condition", structure = "cluster_robust")
  weib <- fit_gaze_aft_model(d, "condition", distribution = "weibull")
  logn <- fit_gaze_aft_model(d, "condition", distribution = "lognormal")
  expect_s3_class(cox, "eye_gaze_survival_model")
  expect_match(clustered$repeated_structure, "cluster_robust")
  expect_warning(cmp <- compare_gaze_survival_models(cox, weib, logn), "not directly comparable")
  expect_equal(nrow(cmp), 3L)
  expect_true(nrow(tidy_gaze_survival_model(cox)) > 0)
  ph <- check_gaze_proportional_hazards(cox)
  expect_true(nrow(ph) > 0)
  expect_true(all(c("alpha", "ph_flag") %in% names(ph)))
  expect_true(all(ph$alpha == .05))
  report <- report_gaze_survival_model(clustered)
  expect_equal(report$N_participants, 40L)
  expect_equal(report$N_review_required, 0L)
  expect_true(length(report$event_type) >= 1L)
  expect_true(length(report$target_aoi) >= 1L)
  expect_true(length(report$time_origin) >= 1L)
})

test_that("PH diagnostic alpha is validated", {
  skip_if_not_installed("survival")
  d <- simulate_gaze_survival_example(n_participants = 20, trials_per_participant = 3)
  cox <- fit_gaze_cox_model(d, "condition")
  expect_error(check_gaze_proportional_hazards(cox, alpha = 0), "strictly between")
  expect_error(check_gaze_proportional_hazards(cox, alpha = 1), "strictly between")
})

test_that("estimators must be selected explicitly", {
  d <- simulate_gaze_survival_example(n_participants = 8, trials_per_participant = 2)
  expect_error(fit_gaze_mixed_cox_model(d, "condition"), "structure.*specified explicitly")
  expect_error(fit_gaze_aft_model(d, "condition"), "distribution.*specified explicitly")
})
test_that("frailty Cox uses the specialist coxme backend", {
  skip_if_not_installed("survival")
  skip_if_not_installed("coxme")
  d <- simulate_gaze_survival_example(n_participants = 40, trials_per_participant = 3)
  frailty <- fit_gaze_mixed_cox_model(d, "condition", structure = "frailty")
  expect_equal(frailty$backend, "coxme::coxme")
  expect_match(frailty$repeated_structure, "gaussian_frailty")
  expect_error(check_gaze_proportional_hazards(frailty), "marginal Cox")
})

test_that("Cox estimates match direct survival backend calls", {
  skip_if_not_installed("survival")
  d <- simulate_gaze_survival_example(seed = 11, n_participants = 50, trials_per_participant = 3)
  ours <- fit_gaze_cox_model(d, "condition")
  direct <- survival::coxph(
    survival::Surv(analysis_time, event_observed) ~ condition,
    data = d, ties = "breslow", x = TRUE, model = TRUE
  )
  expect_equal(unname(stats::coef(ours$fit)), unname(stats::coef(direct)), tolerance = 1e-10)
})

test_that("AFT estimates match direct survival backend calls", {
  skip_if_not_installed("survival")
  d <- simulate_gaze_survival_example(
    seed = 19,
    n_participants = 50,
    trials_per_participant = 3
  )
  for (distribution in c("weibull", "lognormal")) {
    ours <- fit_gaze_aft_model(
      d,
      "condition",
      distribution = distribution
    )
    direct <- survival::survreg(
      survival::Surv(analysis_time, event_observed) ~ condition,
      data = d,
      dist = distribution
    )
    expect_equal(
      unname(stats::coef(ours$fit)),
      unname(stats::coef(direct)),
      tolerance = 1e-10
    )
    expect_equal(ours$fit$scale, direct$scale, tolerance = 1e-10)
    expect_equal(
      as.numeric(stats::logLik(ours$fit)),
      as.numeric(stats::logLik(direct)),
      tolerance = 1e-10
    )
  }
})

test_that("AFT refuses zero-time events", {
  skip_if_not_installed("survival")
  d <- simulate_gaze_survival_example(n_participants = 8)
  d$event_observed[1] <- 1
  d$event_time[1] <- 0
  d$analysis_time[1] <- 0
  expect_error(fit_gaze_aft_model(d, "condition", distribution = "weibull"), "strictly positive")
})

test_that("explicit sensitivity branches retain specification metadata", {
  skip_if_not_installed("survival")
  d <- simulate_gaze_survival_example(seed = 7, n_participants = 30, trials_per_participant = 3)
  alt <- d
  alt$aoi_specification <- "expanded synthetic AOI"
  out <- compare_gaze_survival_specifications(
    list(primary = d, expanded_aoi = alt),
    "condition",
    model_families = c("cox_cluster_robust", "aft_weibull")
  )
  expect_equal(sort(unique(out$specification)), c("expanded_aoi", "primary"))
  expect_true("aoi_specification" %in% names(out))
})

test_that("synthetic survival inputs and prepared tables are deterministic", {
  a <- simulate_gaze_survival_inputs(seed = 123, n_participants = 8, trials_per_participant = 2)
  b <- simulate_gaze_survival_inputs(seed = 123, n_participants = 8, trials_per_participant = 2)
  expect_equal(a, b)
  expect_equal(
    simulate_gaze_survival_example(seed = 123, n_participants = 8, trials_per_participant = 2),
    simulate_gaze_survival_example(seed = 123, n_participants = 8, trials_per_participant = 2)
  )
})

test_that("verification example uses source evidence entry contract", {
  d <- simulate_gaze_survival_example(
    "verification",
    seed = 20260918,
    n_participants = 12,
    trials_per_participant = 3
  )
  expect_equal(unique(d$target_aoi), "source_evidence")
  expect_equal(unique(d$event_type), "first_aoi_entry")
  expect_equal(sort(unique(d$condition)), c("evidence_prompt", "standard"))
  expect_true(any(d$event_observed == 0))
})

test_that("cross-language contract fixture has exact canonical fields", {
  f <- testthat::test_path("..", "..", "inst", "extdata", "gaze_survival_contract.csv")
  d <- utils::read.csv(f, stringsAsFactors = FALSE)
  expect_equal(
    names(d),
    c(
      "participant_id", "trial_id", "stimulus_id", "condition", "target_aoi",
      "time_origin", "event_time", "censor_time", "analysis_time",
      "event_observed", "event_type", "n_valid_samples", "valid_data_fraction",
      "trial_duration"
    )
  )
  expect_equal(sum(d$event_observed == 1), 4L)
  expect_equal(sum(d$event_observed == 0), 2L)
  issues <- validate_gaze_survival_data(d, raise_on_error = FALSE)
  expect_false(any(issues$severity == "error"))
})