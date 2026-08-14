test_that("M2 simulation is deterministic and retains truth", {
  a <- simulate_multimodal_m2(n_person = 30, n_item = 8, seed = 42)
  b <- simulate_multimodal_m2(n_person = 30, n_item = 8, seed = 42)

  expect_identical(a$data, b$data)
  expect_identical(a$truth$theta, b$truth$theta)
  expect_equal(nrow(a$data), 240)
  expect_equal(length(a$truth$theta), 30)
  expect_equal(length(a$truth$b), 8)
  expect_true(all(a$data$response %in% c(0, 1)))
  expect_true(all(a$data$rt > 0))
  expect_true(all(a$data$gaze >= 0))
})

test_that("M2 simulation applies channel dropout after generating complete truth", {
  s <- simulate_multimodal_m2(
    n_person = 40,
    n_item = 8,
    dropout = c(response = .10, rt = .20, gaze = .30),
    seed = 91
  )

  expect_true(anyNA(s$data$response))
  expect_true(anyNA(s$data$rt))
  expect_true(anyNA(s$data$gaze))
  expect_false(anyNA(s$complete_data$response))
  expect_false(anyNA(s$complete_data$rt))
  expect_false(anyNA(s$complete_data$gaze))
})
