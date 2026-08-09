testthat::test_that("pre-action and decision proxy features are generated", {
  set.seed(12)
  d <- expand.grid(person_id = c("P1", "P2"), trial_id = c("T1", "T2"), sample = 1:50)
  d$time_ms <- d$sample * 20
  d$response_time_ms <- 1000
  d$aoi <- rep(c("target", "distractor", "button", "text"), length.out = nrow(d))
  d$pupil_bc <- rnorm(nrow(d))
  d$blink <- FALSE
  p <- preaction_process_features(d, windows_ms = c(500, 1000))
  q <- addm_glam_proxy_features(d)
  testthat::expect_s3_class(p, "eye_preaction_process_features")
  testthat::expect_s3_class(q, "eye_decision_process_proxy")
  testthat::expect_true(nrow(p$data) > 0)
  testthat::expect_true(nrow(q$features) > 0)
})

testthat::test_that("feature stability assigns conservative families", {
  d <- expand.grid(feature = c("pupil_mean", "valid_gaze", "aoi_entropy"), split = 1:4)
  d$importance <- runif(nrow(d))
  s <- process_feature_stability(d, top_n = 2)
  testthat::expect_true(all(c("feature_family", "top_n_selection_rate") %in% names(s)))
})

testthat::test_that("base plot helpers execute on synthetic inputs", {
  tf <- tempfile(fileext = ".pdf")
  grDevices::pdf(tf)
  on.exit({grDevices::dev.off(); unlink(tf)}, add = TRUE)
  trans <- data.frame(from = c("A", "A", "B", "B"), to = c("B", "C", "A", "C"))
  testthat::expect_silent(plot_aoi_transition_matrix(trans))
  testthat::expect_silent(plot_aoi_transition_rank(trans))
  sig <- data.frame(time_ms = 1:20, pupil_raw = rnorm(20), pupil_smoothed = rnorm(20), pupil_bc = rnorm(20))
  testthat::expect_silent(plot_pupil_preprocessing_audit(sig))
})
