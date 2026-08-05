test_that("functional pupil specifications remain neutral", {
  spec <- functional_pupil_irt_spec(engine = "two_stage_glm")
  expect_s3_class(spec, "eye_functional_pupil_irt_spec")
  expect_match(spec$interpretation, "not")
})

test_that("functional bases are stable and finite", {
  time <- seq(-.2, 1.5, length.out = 60)
  basis <- functional_pupil_basis(time, df = 6L, degree = 3L)
  expect_equal(nrow(basis), length(time))
  expect_true(all(is.finite(basis)))
  expect_true(ncol(basis) >= 4L)
})

test_that("pupil preprocessing grid enumerates sensitivities", {
  grid <- pupil_preprocessing_grid(
    baseline_windows = list(c(-200, 0), c(-100, 0)),
    latency_ms = c(100, 200),
    basis_df = c(4L, 6L),
    baseline_methods = c("subtract", "percent"),
    max_interpolated_fraction = c(.1, .2)
  )
  expect_true(nrow(grid) >= 8L)
  expect_equal(nrow(unique(grid)), nrow(grid))
})

test_that("functional pupil Stan program is bundled", {
  expect_true(file.exists(system.file("stan", "functional_pupil_irt.stan", package = "eyeprocess")))
})

test_that("event alignment and baseline uncertainty are explicit", {
  d <- expand.grid(
    participant_id = c("P1", "P2"),
    item_id = c("I1", "I2"),
    sample = seq_len(12),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  d$trial_id <- paste(d$participant_id, d$item_id, sep = "-")
  d$time_ms <- 800 + (d$sample - 1) * 100
  d$event_time <- 1200
  d$pupil <- 3 + 0.02 * d$sample + as.integer(d$item_id == "I2") * 0.1
  d$score <- as.integer(d$item_id == "I2")
  spec <- functional_pupil_irt_spec(
    alignment = "event", event_time_column = "event_time", latency_ms = 0,
    baseline_window = c(-400, 0), min_baseline_samples = 3L,
    pupil_column = "pupil", time_column = "time_ms", engine = "two_stage_glm"
  )
  prepared <- prepare_functional_pupil_data(d, spec)
  expect_s3_class(prepared, "eye_functional_pupil_data")
  expect_true(all(c("baseline_n", "baseline_se", "baseline_valid") %in% names(prepared$data)))
  expect_true(all(prepared$data$baseline_valid))
  expect_true(any(prepared$data$.time < 0) && any(prepared$data$.time > 0))
})

test_that("functional pupil specs reject incomplete event alignment", {
  expect_error(functional_pupil_irt_spec(alignment = "event"), "event_time_column")
  expect_error(functional_pupil_irt_spec(adapt_delta = 1), "adapt_delta")
  expect_error(functional_pupil_irt_spec(parallel_chains = 5, chains = 4), "parallel_chains")
})
