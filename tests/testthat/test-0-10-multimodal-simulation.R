test_that("multimodal simulation is deterministic", {
  a <- simulate_multimodal_irt(n_person=30, n_item=8, seed=99)
  b <- simulate_multimodal_irt(n_person=30, n_item=8, seed=99)
  expect_identical(a$data, b$data)
  expect_identical(a$truth$persons, b$truth$persons)
})

test_that("simulated channels respect support", {
  x <- simulate_multimodal_irt(n_person=30, n_item=8, seed=10)
  expect_true(all(x$data$response %in% c(0L,1L)))
  expect_true(all(x$data$rt > 0))
  expect_true(all(x$data$gaze_fixation_count >= 0))
  expect_true(all(is.finite(x$data$pupil_response)))
  v <- validate_multimodal_irt(x)
  expect_true(v$valid)
})
