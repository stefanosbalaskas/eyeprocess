test_that("explicit 0.7 semantic contracts work without optional engines", {
  contract <- vendor_schema_contract(
    "Gazepoint",
    required_fields = c("timestamp", "x", "y"),
    optional_fields = "pupil",
    timestamp = list(device_time = "timestamp")
  )
  d <- data.frame(timestamp = 1:5, x = 1:5, y = 6:10)
  z <- validate_vendor_semantics(d, contract)
  expect_true(z$pass)

  ev <- data.frame(event_id = 1:3, event = c("start", "stimulus", "response"),
                   timestamp = c(0, 1, 2), HED = c("(Experiment-control)", "(Sensory-event)", "(Agent-action)"))
  er <- event_roundtrip_audit(ev, ev, key = "event_id", hed_column = "HED")
  expect_identical(er$status, "LOSSLESS")
})

test_that("BIDS callback roundtrip and adapter regression are lossless for identity fixtures", {
  d <- data.frame(id = 1:5, timestamp = 1:5, x = seq(.1, .5, .1), y = seq(.2, 1, .2))
  rt <- roundtrip_eye_bids(
    d,
    exporter = function(x) x,
    importer = function(x) x,
    audit_args = list(key = "id", fields = c("timestamp", "x", "y"))
  )
  expect_identical(rt$status, "LOSSLESS")

  ar <- cross_version_adapter_regression(
    d, baseline_adapter = identity, candidate_adapter = identity,
    audit_args = list(key = "id", fields = c("timestamp", "x", "y"))
  )
  expect_identical(ar$status, "LOSSLESS")
})

test_that("validation-replicate helpers produce canonical recovery rows", {
  generator <- function(replicate, scenario) {
    list(data = c(.8, 1.0, 1.2), truth = c(mu = 1))
  }
  fitter <- function(x) mean(x)
  extractor <- function(fit, simulation) {
    data.frame(parameter = "mu", estimate = fit, lower = fit - .2, upper = fit + .2)
  }
  z <- fit_validation_replicate(1, generator, fitter, extractor)
  expect_s3_class(z, "eye_irt_recovery_results")
  expect_equal(z$truth, 1)
  expect_equal(z$estimate, 1)
  expect_true(z$converged)
})

test_that("latent-distribution diagnostics return finite summaries", {
  set.seed(42)
  x <- rnorm(200)
  a <- audit_latent_distribution(x)
  expect_true(is.finite(a$normal_qq_correlation))
  cmp <- compare_latent_distribution_models(x)
  expect_s3_class(cmp, "eye_latent_distribution_comparison")
  expect_true(all(c("normal", "student_t", "two_normal_mixture") %in% cmp$comparison$model))
})

test_that("gaze-informed missingness diagnostic labels its proxy explicitly", {
  set.seed(1)
  d <- expand.grid(participant_id = paste0("P", 1:12), item_id = paste0("I", 1:5),
                   KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  d$gaze_exposure <- rexp(nrow(d), rate = 1)
  d$response <- rbinom(nrow(d), 1, .65)
  d$response[seq(3, nrow(d), by = 11)] <- NA
  z <- fit_gaze_informed_missingness_irt(d)
  expect_s3_class(z, "eye_gaze_informed_missingness_irt")
  expect_identical(z$theta_source, "smoothed-person-score-proxy")
  expect_identical(z$status, "reference-diagnostic")
})
