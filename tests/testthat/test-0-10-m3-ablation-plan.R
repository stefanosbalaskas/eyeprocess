test_that("M3 ablation lattice contains all eight response-anchored channel sets", {
  d <- .ep10_m3_ablation_definitions()
  expect_equal(nrow(d), 8)
  expect_setequal(d$model, c("R","R_RT","R_GAZE","R_PUPIL","R_RT_GAZE","R_RT_PUPIL","R_GAZE_PUPIL","FULL"))
  expect_true(all(grepl("response", d$channels)))
  expect_equal(sum(d$pupil), 4)
  expect_equal(sum(d$rt), 4)
  expect_equal(sum(d$gaze), 4)
})

test_that("M3 full and ablation pupil nuisance selections use one contract", {
  sim <- simulate_multimodal_m3(
    n_person = 25, n_item = 6, pupil_missingness = "none",
    dropout = c(response=0, rt=0, gaze=0, pupil=0), seed = 21
  )
  dat <- .ep10_m3_as_data(sim)
  nuisance <- stats::setNames(c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE), .ep10_m3_nuisance_names)
  full <- .ep10_m3_to_stan(dat, nuisance = nuisance)
  subset <- .ep10_m3_subset_stan_data(
    dat, use_rt = TRUE, use_gaze = TRUE, use_pupil = TRUE,
    nuisance = nuisance
  )
  expect_identical(full$use_pupil_covariate, subset$use_pupil_covariate)
  expect_identical(as.logical(full$use_pupil_covariate), unname(nuisance))

  no_pupil <- .ep10_m3_subset_stan_data(
    dat, use_rt = TRUE, use_gaze = TRUE, use_pupil = FALSE,
    nuisance = nuisance
  )
  expect_true(all(no_pupil$use_pupil_covariate == 0L))
})
