test_that("0.9 validation design expands and runs", {
  d <- process_validation_design(n_persons = 20, n_trials = 8, missingness = 0,
                                 sampling_rate_hz = 60, aoi_error = "low", calibration_error = 0,
                                 pupil_dropout = 0, heterogeneity = "low",
                                 model_misspecification = FALSE, replications = 2, seed = 9)
  g <- expand_process_validation_design(d)
  expect_s3_class(d, "eye_process_validation_design")
  expect_equal(nrow(g), 1)
  x <- run_process_validation(g)
  expect_s3_class(x, "eye_process_validation_result")
  expect_true(nrow(x$estimates) > 0)
  expect_true(all(c("bias", "rmse") %in% names(summarise_process_validation(x))))
  ref <- freeze_validation_reference(x)
  cmp <- validate_against_reference(x, ref, tolerance = 1e-8)
  expect_true(cmp$pass)
})

test_that("0.9 misspecification stress changes the neutral DGP", {
  d <- process_validation_design(
    n_persons = 20, n_trials = 8, missingness = 0,
    sampling_rate_hz = 60, aoi_error = "low", calibration_error = 0,
    pupil_dropout = 0, heterogeneity = "low",
    model_misspecification = c(FALSE, TRUE), replications = 1, seed = 19
  )
  g <- expand_process_validation_design(d)
  expect_equal(nrow(g), 2)
  sim_ok <- simulate_process_validation_data(g[g$model_misspecification == FALSE, , drop = FALSE],
                                             replication = 1, seed = 919)
  sim_miss <- simulate_process_validation_data(g[g$model_misspecification == TRUE, , drop = FALSE],
                                               replication = 1, seed = 919)
  expect_true("omitted_structure" %in% names(sim_miss$data))
  expect_equal(sim_ok$data$omitted_structure, sim_miss$data$omitted_structure)
  expect_false(isTRUE(all.equal(sim_ok$data$process_value, sim_miss$data$process_value)))
})

test_that("0.9 frozen validation references require complete key matching", {
  d <- process_validation_design(
    n_persons = 20, n_trials = 8, missingness = 0,
    sampling_rate_hz = 60, aoi_error = "low", calibration_error = 0,
    pupil_dropout = 0, heterogeneity = "low",
    model_misspecification = FALSE, replications = 2, seed = 29
  )
  x <- run_process_validation(d)
  ref <- freeze_validation_reference(x)
  extra <- ref$summary[1, , drop = FALSE]
  extra$condition_id <- "C_MISSING_FROM_CURRENT"
  ref$summary <- rbind(ref$summary, extra)
  cmp <- validate_against_reference(x, ref, tolerance = 1e-8)
  expect_false(cmp$pass)
  expect_true(any(is.na(cmp$table$.present_current)))
})
