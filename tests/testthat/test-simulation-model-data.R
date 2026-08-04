test_that("simulation produces linked response and process data", {
  x <- simulate_eye_dataset(n_person = 8, n_item = 5, sampling_rate = 20, trial_duration = .5, seed = 5)
  expect_equal(length(unique(x$responses$participant_id)), 8)
  expect_equal(length(unique(x$responses$item_id)), 5)
  expect_true(all(is.finite(x$responses$response_time)))
  m <- response_matrix(x)
  rt <- response_time_matrix(x)
  expect_equal(dim(m), c(8, 5))
  expect_equal(dim(rt), c(8, 5))

  process <- simulate_process_irt(n_person = 8, n_item = 5, seed = 6)
  expect_true(all(c("data", "truth") %in% names(process)))
  expect_equal(nrow(process$data), 40)
})
