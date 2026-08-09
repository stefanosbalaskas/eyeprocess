testthat::test_that("preflight governance returns explicit review decisions", {
  set.seed(1)
  d <- data.frame(
    person_id = rep(paste0("P", 1:8), each = 6),
    valid_gaze_prop = c(rep(.95, 36), rep(.55, 12)),
    valid_pupil_prop = runif(48, .75, .98),
    missing_gaze = c(rep(0, 36), rep(1, 12)),
    missing_pupil = 0,
    rt_ms = rep(900, 48),
    blink_cluster_count = rpois(48, 1),
    sampling_rate_hz = 60
  )
  a <- audit_biometric_preflight(d)
  testthat::expect_s3_class(a, "eye_biometric_preflight")
  testthat::expect_equal(nrow(preflight_decisions(a)), 8)
  testthat::expect_true(any(preflight_decisions(a)$preflight_decision != "pass_preflight"))
  testthat::expect_true(nrow(preflight_exclusion_manifest(a)) == 8)
})

testthat::test_that("multivariate anomaly audit is review-oriented", {
  set.seed(2)
  d <- data.frame(person_id = paste0("P", 1:40),
                  dwell_ms = c(rnorm(39, 700, 50), 2000),
                  rt_ms = c(rnorm(39, 1000, 70), 4000),
                  valid_gaze_prop = c(runif(39, .9, .99), .5))
  a <- audit_process_anomalies(d, metrics = c("dwell_ms", "rt_ms", "valid_gaze_prop"), aggregate = FALSE)
  testthat::expect_s3_class(a, "eye_process_anomaly_audit")
  testthat::expect_true("review_required" %in% names(a$table))
  testthat::expect_match(a$caveat, "not evidence", ignore.case = TRUE)
})

testthat::test_that("deployment drift detects designed difficulty change", {
  d <- expand.grid(item_id = c("i1", "i2", "i3"), deployment_batch = 1:4)
  d$irt_difficulty <- c(rep(0, 3), rep(.05, 3), rep(.10, 3), c(.8, .1, .1))
  d$irt_discrimination <- 1
  d$dwell_ms <- 800
  d$valid_gaze_prop <- .95
  a <- audit_process_drift(d, metrics = c("irt_difficulty", "irt_discrimination", "dwell_ms", "valid_gaze_prop"))
  testthat::expect_s3_class(a, "eye_process_drift_audit")
  testthat::expect_true(any(a$table$difficulty_drift_flag))
  testthat::expect_true(nrow(process_drift_alerts(a)) >= 1)
})


testthat::test_that("grouped process APIs reject missing identifiers rather than silently dropping rows", {
  d <- data.frame(person_id = c("P1", NA), valid_gaze_prop = c(.95, .90))
  testthat::expect_error(
    audit_biometric_preflight(d),
    "Grouping columns must not contain missing values"
  )
})
