edm_ivt <- function(id = "ivt30", threshold = 30) {
  define_event_detector_spec(
    id, "ivt",
    velocity_threshold = threshold,
    minimum_duration_ms = 60,
    maximum_gap_ms = 75,
    sampling_rate = 60,
    coordinate_unit = "degrees"
  )
}

edm_idt <- function(id = "idtA") {
  define_event_detector_spec(
    id, "idt",
    dispersion_threshold = 1.2,
    minimum_duration_ms = 80,
    sampling_rate = 60,
    coordinate_unit = "degrees"
  )
}

edm_adaptive <- function(id = "adaptive") {
  define_event_detector_spec(
    id, "adaptive_velocity",
    minimum_duration_ms = 60,
    maximum_gap_ms = 75,
    sampling_rate = 60,
    parameters = list(
      noise_factor = 4,
      minimum_velocity_threshold = 20
    )
  )
}

test_that("detector specification validates scientific inputs", {
  expect_error(
    define_event_detector_spec(
      "bad", "ivt",
      velocity_threshold = 30,
      minimum_duration_ms = 60
    ),
    "sampling_rate"
  )
  expect_error(
    define_event_detector_spec(
      "bad", "ivt",
      velocity_threshold = -1,
      minimum_duration_ms = 60,
      sampling_rate = 60
    ),
    "velocity_threshold"
  )
  expect_error(
    define_event_detector_spec(
      "bad", "idt",
      dispersion_threshold = 0,
      minimum_duration_ms = 80,
      sampling_rate = 60
    ),
    "dispersion_threshold"
  )
  expect_error(
    define_event_detector_spec("bad", "external"),
    "callback"
  )
})

test_that("parameter grids are explicit and deterministic", {
  mv <- create_detector_multiverse(
    base_spec = edm_ivt("ivt"),
    parameter_grid = list(
      velocity_threshold = c(20, 25, 30, 35, 40),
      minimum_duration_ms = c(60, 80)
    )
  )
  expect_s3_class(mv, "eye_detector_multiverse")
  expect_equal(length(mv$specs), 10L)
  expect_identical(
    vapply(mv$specs, `[[`, character(1), "detector_id"),
    sort(vapply(mv$specs, `[[`, character(1), "detector_id"))
  )
  expect_setequal(
    vapply(mv$specs, `[[`, numeric(1), "velocity_threshold"),
    c(20, 25, 30, 35, 40)
  )
})

test_that("shared contract fixture has the same event matching semantics", {
  skip_if_not_installed("jsonlite")
  fixture <- jsonlite::fromJSON(
    test_path("fixtures", "detector_multiverse_contract.json"),
    simplifyVector = TRUE
  )
  ivt <- do.call(
    define_event_detector_spec,
    c(
      fixture$specs[1, c(
        "detector_id", "algorithm", "velocity_threshold",
        "minimum_duration_ms", "maximum_gap_ms",
        "merge_rule", "sampling_rate", "coordinate_unit",
        "implementation", "implementation_version"
      )],
      list(parameters = list())
    )
  )
  idt <- do.call(
    define_event_detector_spec,
    c(
      fixture$specs[2, c(
        "detector_id", "algorithm", "dispersion_threshold",
        "minimum_duration_ms", "merge_rule", "sampling_rate",
        "coordinate_unit", "implementation", "implementation_version"
      )],
      list(parameters = list())
    )
  )
  expect_equal(ivt$velocity_threshold, 30)
  expect_equal(idt$dispersion_threshold, 1.2)

  reference <- fixture$event_matching_fixture$reference
  candidate <- fixture$event_matching_fixture$candidate
  out <- compare_event_catalogues(reference, candidate)
  expect_equal(out$reference_events, 2)
  expect_equal(out$candidate_events, 3)
  expect_equal(out$matched_events, 2)
  expect_equal(out$matched_event_precision, 2 / 3)
  expect_equal(out$matched_event_recall, 1)
})

test_that("specification order does not change scientific outputs", {
  d <- simulate_detector_multiverse_data(n_participants = 4, seed = 11)
  specs <- list(edm_ivt("b", 30), edm_idt("a"), edm_adaptive("c"))

  one <- run_detector_multiverse(
    d,
    create_detector_multiverse(specs)
  )
  two <- run_detector_multiverse(
    d,
    create_detector_multiverse(rev(specs))
  )

  expect_equal(one$status, two$status)
  cols <- c(
    "detector_id", "episode_type", "recording_id", "trial_id",
    "start_time", "end_time", "duration_ms"
  )
  a <- one$events[, cols, drop = FALSE]
  b <- two$events[, cols, drop = FALSE]
  ord_a <- do.call(order, a)
  ord_b <- do.call(order, b)
  expect_equal(a[ord_a, , drop = FALSE], b[ord_b, , drop = FALSE])
})

test_that("identical scientific specs yield identical event catalogues", {
  d <- simulate_detector_multiverse_data(n_participants = 4, seed = 12)
  out <- run_detector_multiverse(
    d,
    list(edm_ivt("x", 30), edm_ivt("y", 30))
  )
  x <- out$events[out$events$detector_id == "x", ]
  y <- out$events[out$events$detector_id == "y", ]
  cols <- c(
    "recording_id", "trial_id", "episode_type",
    "start_time", "end_time", "duration_ms",
    "centroid_x", "centroid_y"
  )
  expect_equal(
    unname(x[, cols, drop = FALSE]),
    unname(y[, cols, drop = FALSE])
  )
})

test_that("short and zero-event trials are retained in features", {
  d <- simulate_detector_multiverse_data(
    n_participants = 4,
    trial_duration_s = .08,
    seed = 13
  )
  out <- run_detector_multiverse(d, list(edm_ivt()))
  out <- propagate_detector_to_aoi(out)
  out <- propagate_detector_to_features(out)

  n_trials <- sum(d$intervals$interval_type == "trial")
  expect_equal(
    nrow(out$features),
    n_trials * nrow(d$aoi_definitions)
  )
  expect_true(any(replace(out$features$fixation_count, is.na(out$features$fixation_count), 0) == 0))
})

test_that("extreme missingness is not converted to zero", {
  d <- simulate_detector_multiverse_data(n_participants = 4, seed = 14)
  d$gaze_samples$valid <- FALSE

  out <- run_detector_multiverse(d, list(edm_ivt()))
  out <- propagate_detector_to_aoi(out)
  out <- propagate_detector_to_features(out)

  expect_true(all(out$features$valid_data_fraction == 0))
  expect_true(all(is.na(out$features$fixation_count)))
  expect_true(all(is.na(out$features$dwell_time_ms)))
  expect_true(all(out$features$feature_review_required))
})

test_that("constant and dense-motion gaze are handled explicitly", {
  constant <- simulate_detector_multiverse_data(n_participants = 4, seed = 15)
  constant$gaze_samples$gaze_x <- 6
  constant$gaze_samples$gaze_y <- 2.4
  constant$gaze_samples$valid <- TRUE

  out <- run_detector_multiverse(constant, list(edm_ivt()))
  expect_identical(out$status$status, "ok")
  expect_gt(out$status$n_fixations, 0)

  dense <- simulate_detector_multiverse_data(n_participants = 4, seed = 16)
  idx <- seq_len(nrow(dense$gaze_samples))
  dense$gaze_samples$gaze_x <- ifelse(idx %% 2L, 10, 0)
  dense$gaze_samples$gaze_y <- ifelse(idx %% 2L, 0, 8)
  dense$gaze_samples$valid <- TRUE

  dense_out <- run_detector_multiverse(dense, list(edm_ivt("strict", 20)))
  dense_out <- propagate_detector_to_aoi(dense_out)
  dense_out <- propagate_detector_to_features(dense_out)
  expect_equal(
    nrow(dense_out$features),
    nrow(dense$intervals) * nrow(dense$aoi_definitions)
  )
  expect_true(all(dense_out$features$fixation_count == 0))
})

test_that("external callbacks and failures are auditable", {
  d <- simulate_detector_multiverse_data(n_participants = 4, seed = 17)

  good_callback <- function(data, spec) {
    tr <- data$intervals[1, ]
    data.frame(
      recording_id = tr$recording_id,
      trial_id = tr$trial_id,
      episode_type = "fixation",
      start_time = tr$start_time + .1,
      end_time = tr$start_time + .2,
      centroid_x = 6,
      centroid_y = 2.4,
      coordinate_space_id = "deg_display",
      stimulus_id = "stim_01",
      stringsAsFactors = FALSE
    )
  }

  good <- define_event_detector_spec(
    "external_good", "external",
    implementation = "test_callback",
    callback = good_callback
  )
  out <- run_detector_multiverse(d, list(good))
  expect_equal(nrow(out$events), 1)
  expect_identical(out$events$detector_id, "external_good")

  bad <- define_event_detector_spec(
    "external_bad", "external",
    implementation = "test_callback",
    callback = function(data, spec) stop("detector exploded")
  )
  failed <- run_detector_multiverse(d, list(bad))
  expect_identical(failed$status$status, "failed")
  expect_match(failed$failures$error, "detector exploded")
})

test_that("AOI ambiguity requires an explicit resolution rule", {
  d <- simulate_detector_multiverse_data(n_participants = 4, seed = 18)
  d <- register_aois(
    d,
    new_aoi(
      "overlap", "Overlap", "stim_01", "rectangle",
      x = 4.5, y = 1.5, width = 3, height = 2,
      coordinate_space_id = "deg_display"
    )
  )
  out <- run_detector_multiverse(d, list(edm_ivt()))

  expect_error(
    propagate_detector_to_aoi(
      out,
      overlap = "error",
      continue_on_error = FALSE
    ),
    "Ambiguous AOI"
  )
  resolved <- propagate_detector_to_aoi(
    out,
    overlap = "smallest",
    continue_on_error = FALSE
  )
  expect_equal(nrow(resolved$failures), 0)
})

test_that("event and feature provenance retain the analytical branch", {
  d <- simulate_detector_multiverse_data(n_participants = 4, seed = 19)
  out <- run_detector_multiverse(d, list(edm_ivt()))

  needed <- c(
    "source_data_hash", "preprocessing_provenance_hash",
    "aoi_spec_hash", "detector_spec_hash", "software",
    "software_version"
  )
  expect_true(all(needed %in% names(out$events)))
  expect_false(anyNA(out$events$detector_spec_hash))

  out <- propagate_detector_to_aoi(out)
  out <- propagate_detector_to_features(out)
  expect_true(all(needed %in% names(out$features)))
  expect_false(anyNA(out$features$source_data_hash))
})

test_that("synthetic truth produces positive disclosure dwell across branches", {
  d <- simulate_detector_multiverse_data(n_participants = 8, seed = 20)
  out <- run_detector_multiverse(
    d,
    list(
      edm_ivt("ivt25", 25),
      edm_ivt("ivt35", 35),
      edm_idt(),
      edm_adaptive()
    )
  )
  out <- propagate_detector_to_aoi(out)
  out <- propagate_detector_to_features(out)

  target <- out$features[out$features$aoi_id == "disclosure", ]
  means <- stats::aggregate(
    dwell_time_ms ~ detector_id + condition_id,
    target,
    mean
  )
  wide <- reshape(
    means,
    idvar = "detector_id",
    timevar = "condition_id",
    direction = "wide"
  )
  expect_true(
    all(wide$dwell_time_ms.disclosure > wide$dwell_time_ms.control)
  )
})

test_that("inference stores coefficients and gates non-convergence", {
  d <- simulate_detector_multiverse_data(n_participants = 6, seed = 21)
  out <- run_detector_multiverse(
    d,
    list(
      edm_ivt("ivt25", 25),
      edm_ivt("ivt35", 35),
      edm_idt()
    )
  )
  out <- propagate_detector_to_aoi(out)
  out <- propagate_detector_to_features(out)

  model_spec <- list(
    engine = "stats_lm",
    formula = dwell_time_ms ~ condition_id + participant_id,
    outcome = "dwell_time_ms",
    aoi_id = "disclosure"
  )
  fit <- run_detector_inference_multiverse(out, model_spec)
  term <- grep("condition_id", fit$coefficients$term, value = TRUE)[1]

  stability <- assess_detector_inference_stability(
    fit,
    term = term,
    substantive_threshold = 100
  )
  expect_equal(stability$convergence_rate, 1)
  expect_equal(stability$same_sign_proportion, 1)
  expect_equal(stability$substantive_conclusion_stability, 1)
  expect_true(
    all(c("model_spec_hash", "feature_fingerprint") %in%
      names(fit$coefficients))
  )

  nonconverged <- function(data, model_spec) {
    data.frame(
      term = "condition",
      estimate = 1,
      SE = 1,
      CI_lower = -1,
      CI_upper = 3,
      p = .5,
      converged = FALSE,
      N = nrow(data),
      stringsAsFactors = FALSE
    )
  }
  bad_spec <- model_spec
  bad_spec$engine <- "callback"
  bad <- run_detector_inference_multiverse(
    out,
    bad_spec,
    model_callback = nonconverged
  )
  bad_summary <- assess_detector_inference_stability(
    bad,
    term = "condition"
  )
  expect_equal(bad_summary$converged_specifications, 0)
  expect_equal(bad_summary$convergence_rate, 0)
})

test_that("formula missingness fails instead of silently dropping rows", {
  d <- simulate_detector_multiverse_data(n_participants = 4, seed = 22)
  out <- run_detector_multiverse(d, list(edm_ivt()))
  out <- propagate_detector_to_aoi(out)
  out <- propagate_detector_to_features(out)
  out$features$condition_id[1] <- NA_character_

  fit <- run_detector_inference_multiverse(
    out,
    list(
      engine = "stats_lm",
      formula = dwell_time_ms ~ condition_id,
      outcome = "dwell_time_ms",
      aoi_id = "disclosure"
    )
  )
  expect_gt(nrow(fit$failures), 0)
  expect_equal(nrow(fit$coefficients), 0)
})

test_that("reporting and plot surfaces execute", {
  d <- simulate_detector_multiverse_data(n_participants = 4, seed = 23)
  out <- run_detector_multiverse(
    d,
    list(
      edm_ivt("ivt25", 25),
      edm_ivt("ivt35", 35),
      edm_idt()
    )
  )
  out <- propagate_detector_to_aoi(out)
  out <- propagate_detector_to_features(out)

  path <- tempfile(fileext = ".md")
  text <- report_detector_multiverse(out, path = path)
  expect_match(text, "Do not summarize robustness by counting p-values alone")
  expect_true(file.exists(path))

  pdf <- tempfile(fileext = ".pdf")
  grDevices::pdf(pdf)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_invisible(plot_detector_event_timeline(out))
  expect_invisible(plot_detector_agreement(out))
  expect_invisible(
    plot_detector_feature_distributions(
      out,
      aoi_id = "disclosure"
    )
  )
})

test_that("REMoDNaV bridge fails explicitly when command is unavailable", {
  d <- simulate_detector_multiverse_data(n_participants = 4, seed = 24)
  spec <- define_event_detector_spec(
    "remodnav_missing", "remodnav",
    minimum_duration_ms = 60,
    sampling_rate = 60,
    parameters = list(
      noise_factor = 5,
      command = "eyeprocess-definitely-no-remodnav"
    )
  )
  out <- run_detector_multiverse(d, list(spec))
  expect_identical(out$status$status, "failed")
  expect_match(out$failures$error, "REMoDNaV executable was not found")
})
