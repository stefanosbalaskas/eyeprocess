test_that("pupil latency sensitivity exposes estimator spread", {
  time <- seq(-0.5, 2, by = 0.01)
  pupil <- ifelse(time < 0.30, 4, 4 - 0.8 * (1 - exp(-(time - 0.30) / 0.20)))
  out <- eyeprocess:::pupil_latency_sensitivity(time, pupil)
  expect_true(out$latency_resolvability %in% c("high", "moderate", "low"))
  expect_true(is.finite(out$estimates_s[["sustained_threshold"]]))
  expect_equal(out$sampling_hz, 100, tolerance = 1e-6)
})

test_that("event marker QC does not claim clock synchronization", {
  out <- eyeprocess:::event_marker_qc(c(0.01, 0.015, 0.02), tolerance = 0.05)
  expect_equal(out$status, "confirmed")
  expect_match(out$note, "no clock-drift correction")
})

test_that("generalization is gated by held-out-person evidence", {
  out <- eyeprocess:::validation_ladder(
    acquisition_qc = "pass", analytical_qc = "pass", construct_check = "pass",
    within_person = "pass", held_out_person = "not_assessed", claim = "generalizable"
  )
  expect_equal(out$claim_status, "not_supported")
  expect_false(out$held_out_person_generalization)
})
