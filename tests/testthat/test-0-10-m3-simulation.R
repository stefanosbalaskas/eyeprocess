test_that("M3 simulation is deterministic and retains complete truth", {
  a <- simulate_multimodal_m3(n_person = 30, n_item = 8, seed = 20260815, pupil_missingness = "none", dropout = c(response=0,rt=0,gaze=0,pupil=0))
  b <- simulate_multimodal_m3(n_person = 30, n_item = 8, seed = 20260815, pupil_missingness = "none", dropout = c(response=0,rt=0,gaze=0,pupil=0))
  expect_s3_class(a, "eye_multimodal_m3_simulation")
  expect_identical(a$data, b$data)
  expect_equal(nrow(a$data), 240)
  expect_true(all(c("pupil_baseline", "luminance", "gaze_x", "gaze_y", "pupil_quality", "pupil_blink", "pupil_interpolated", "time_on_task", "device", "session") %in% names(a$data)))
  expect_true(all(c("theta", "tau", "omega", "rho", "b", "beta", "m", "kappa") %in% names(a$truth)))
  expect_equal(sum(is.na(a$data$pupil)), 0)
})

test_that("M3 simulation includes scientific null/confounded pupil cases", {
  null <- simulate_multimodal_m3(n_person=25,n_item=6,pupil_signal="null",pupil_missingness="none",dropout=c(response=0,rt=0,gaze=0,pupil=0),seed=1)
  conf <- simulate_multimodal_m3(n_person=25,n_item=6,pupil_signal="confounded",pupil_missingness="none",dropout=c(response=0,rt=0,gaze=0,pupil=0),seed=2)
  expect_gt(stats::sd(null$truth$rho), 0)
  expect_gt(stats::sd(null$truth$kappa), 0)
  expect_true(all(abs(null$truth$cor_person[4, 1:3]) < 1e-12))
  expect_true(all(conf$truth$rho == 0))
  expect_true(all(conf$truth$kappa == 0))
  expect_gt(stats::sd(conf$complete_data$pupil_nuisance_effect), 0)
})

test_that("M3 nuisance scaling is not redefined by pupil-outcome dropout", {
  sim <- simulate_multimodal_m3(
    n_person = 30, n_item = 8, pupil_missingness = "none",
    dropout = c(response=0, rt=0, gaze=0, pupil=0), seed = 31
  )
  full <- .ep10_m3_as_data(sim$data, pupil_scale = "raw")
  dropped <- sim$data
  dropped$pupil[seq(1, nrow(dropped), by = 4)] <- NA_real_
  partial <- .ep10_m3_as_data(dropped, pupil_scale = "raw")
  expect_equal(full$nuisance$center, partial$nuisance$center, tolerance = 1e-12)
  expect_equal(full$nuisance$scale, partial$nuisance$scale, tolerance = 1e-12)
  expect_equal(colnames(full$nuisance_matrix), .ep10_m3_nuisance_names)
  expect_length(.ep10_m3_nuisance_names, 8L)
})
