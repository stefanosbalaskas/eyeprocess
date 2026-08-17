test_that("M3 support audit accepts a connected four-channel design", {
  sim <- simulate_multimodal_m3(n_person=40,n_item=8,pupil_missingness="none",dropout=c(response=0,rt=0,gaze=0,pupil=0),seed=4)
  a <- audit_multimodal_m3_identifiability(sim)
  expect_s3_class(a, "eye_multimodal_m3_identifiability")
  expect_true(a$supported)
  expect_true(all(a$variation))
  expect_equal(names(a$missing_fraction), c("response","rt","gaze","pupil"))
})

test_that("M3 never silently imputes supplied pupil confounds", {
  sim <- simulate_multimodal_m3(n_person=25,n_item=6,pupil_missingness="none",dropout=c(response=0,rt=0,gaze=0,pupil=0),seed=5)
  d <- sim$data
  d$pupil_baseline[1] <- NA_real_
  expect_error(audit_multimodal_m3_identifiability(d), "does not silently impute")
})
