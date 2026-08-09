testthat::test_that("process windows produce temporal features", {
  set.seed(3)
  d <- expand.grid(person_id = c("P1", "P2"), trial_id = c("T1", "T2"), sample = 0:59)
  d$time_ms <- d$sample * 50
  d$pupil_bc <- sin(d$time_ms / 600) + rnorm(nrow(d), 0, .05)
  d$x <- rnorm(nrow(d), 500, 30)
  d$y <- rnorm(nrow(d), 400, 30)
  d$aoi <- rep(c("target", "text", "button"), length.out = nrow(d))
  d$valid_gaze_prop <- .95
  d$valid_pupil_prop <- .95
  d$blink <- FALSE
  d$trackloss <- FALSE
  x <- extract_process_windows(d, person = "person_id", trial = "trial_id", time = "time_ms",
                               spec = process_window_spec(1000, 500, 0, 3000), pupil = "pupil_bc")
  testthat::expect_s3_class(x, "eye_process_windows")
  testthat::expect_true(nrow(x$data) > 0)
  testthat::expect_true(all(c("pupil_mean", "aoi_entropy", "gaze_path_length") %in% names(x$data)))
  testthat::expect_true(validate_process_windows(x)$valid)
})

testthat::test_that("pupil activity features are finite on a synthetic oscillation", {
  t <- seq(0, 5000, by = 1000 / 60)
  y <- sin(2 * pi * .3 * t / 1000) + .2 * sin(2 * pi * 1.2 * t / 1000)
  lo <- pupil_band_power(y, 60, .05, .5)
  hi <- pupil_band_power(y, 60, .5, 4)
  testthat::expect_true(is.finite(lo) && is.finite(hi))
  testthat::expect_true(is.finite(pupil_velocity_activity(y, t)))
  testthat::expect_true(is.finite(pupil_activity_index(y, t, 60, method = "frequency_contrast")))
})

testthat::test_that("pupil event deconvolution recovers a positive event effect", {
  set.seed(4)
  t <- seq(0, 3000, by = 20)
  k <- pupil_response_kernel(t - 500)
  d <- data.frame(person_id = "P1", trial_id = "T1", time_ms = t,
                  pupil_bc = 0.8 * k + rnorm(length(t), 0, .02), event_ms = 500)
  fit <- fit_pupil_event_deconvolution(d, events = list(stimulus = "event_ms"))
  testthat::expect_s3_class(fit, "eye_pupil_deconvolution")
  testthat::expect_true(nrow(fit$effects) == 1)
  testthat::expect_gt(fit$effects$beta__stimulus, 0)
})

testthat::test_that("base signal filter remains auditable", {
  set.seed(5)
  y <- sin(seq(0, 6, length.out = 101)) + rnorm(101, 0, .1)
  y[50] <- y[50] + 3
  f <- filter_eye_signal(y, width = 9, method = "runmed")
  testthat::expect_s3_class(f, "eye_signal_filter_audit")
  testthat::expect_true(is.finite(audit_signal_filter(f)$filtered_sd))
})


testthat::test_that("pupil confound LM fallback adapts to limited predictor support", {
  set.seed(51)
  n <- 60
  d <- data.frame(
    person_id = rep(paste0("P", 1:10), each = 6),
    item_id = rep(paste0("I", 1:6), times = 10),
    pupil_peak = rnorm(n),
    screen_luminance = rep(c(80, 160), length.out = n),
    trial_sequence = rep(1:3, length.out = n)
  )
  fit <- fit_pupil_confound_model(d, engine = "lm")
  testthat::expect_s3_class(fit, "eye_pupil_confound_model")
  testthat::expect_equal(fit$engine, "lm")
  testthat::expect_true(all(is.finite(fit$data$pupil_confound_adjusted)))
})

testthat::test_that("empty grouped feature inputs fail explicitly", {
  d <- data.frame(person_id = character(), trial_id = character(), time_ms = numeric(),
                  pupil_bc = numeric(), aoi = character())
  testthat::expect_error(pupil_frequency_features(d), "at least one row")
  testthat::expect_error(aoi_trajectory_features(d), "at least one row")
})
