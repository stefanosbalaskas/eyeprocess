test_that("stress, reliability, and negative-control evidence plans expand", {
  sp <- eyeprocess_stress_evidence_plan(missing_gaze=c(0,.1), pupil_dropout=0, calibration_offset=0,
                                        sampling_jitter=0,aoi_label_noise=0,device_shift=0,trial_imbalance=0,seed=4)
  sg <- expand_eyeprocess_stress_evidence_plan(sp)
  expect_true(nrow(sg) >= 8L)
  expect_s3_class(eyeprocess_reliability_evidence_plan(bootstrap=10), "eye_reliability_evidence_plan")
  expect_s3_class(eyeprocess_negative_control_evidence_plan(replications=10), "eye_negative_control_evidence_plan")
})

test_that("frozen evidence detects tampering", {
  claims <- eyeprocess_validation_claim_matrix("C1","software behavior is reproducible","E1","test","supported")
  fr <- freeze_eyeprocess_validation_evidence(design=data.frame(id=1), recovery=data.frame(x=1), stress=data.frame(x=1),
                                               reliability=data.frame(x=1), negative_controls=data.frame(x=1),
                                               claims=claims, provenance=list(commit="abc"), source_commit="abc")
  expect_true(verify_eyeprocess_validation_evidence(fr))
  bad <- fr; bad$components$recovery$x <- 2
  expect_false(verify_eyeprocess_validation_evidence(bad))
})

test_that("executed stress evidence is deterministic and captures metric deltas", {
  d <- data.frame(x = seq_len(40), keep = TRUE)
  plan <- eyeprocess_stress_evidence_plan(missing_gaze=c(0,.1), pupil_dropout=0, calibration_offset=0,
                                          sampling_jitter=0,aoi_label_noise=0,device_shift=0,trial_imbalance=0,seed=11)
  corruptors <- lapply(setdiff(names(plan), "seed"), function(nm) function(data,severity,seed) { data$x <- data$x + severity; data })
  names(corruptors) <- setdiff(names(plan), "seed")
  metric <- function(z) c(mean_x = mean(z$x))
  a <- run_eyeprocess_stress_evidence(d, plan, corruptors, metric)
  b <- run_eyeprocess_stress_evidence(d, plan, corruptors, metric)
  expect_s3_class(a, "eye_stress_evidence_result")
  expect_identical(a$results, b$results)
  expect_equal(nrow(a$failures), 0L)
  expect_true(nrow(summarise_eyeprocess_stress_evidence(a)) > 0L)
})

test_that("claim matrix has a scalar default status and rejects accidental recycling", {
  x <- eyeprocess_validation_claim_matrix("C1","claim","E1","test")
  expect_equal(nrow(x), 1L)
  expect_equal(x$status, "qualified")
  expect_error(eyeprocess_validation_claim_matrix(c("C1","C2"), c("a","b","c"), "E", "test"))
})
