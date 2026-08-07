test_that("recurrence features are bounded and windowed", {
  gaze <- mi_gaze_data(60)
  recurrence <- gaze_recurrence(gaze, x_col = "x", y_col = "y")
  expect_s3_class(recurrence, "eye_recurrence")
  expect_true(recurrence$summary$recurrence_rate >= 0 && recurrence$summary$recurrence_rate <= 1)
  cross <- cross_recurrence(gaze$x, gaze$pupil)
  expect_s3_class(cross, "eye_cross_recurrence")
  windowed <- windowed_recurrence(recurrence, window = 20, step = 10)
  expect_true(nrow(windowed$summary) >= 4)
  expect_plot_silent(plot_recurrence_matrix(recurrence))
})

test_that("cross recurrence preserves one-column matrix inputs", {
  x <- matrix(seq_len(30), ncol = 1)
  y <- matrix(sin(seq_len(30) / 4), ncol = 1)
  fit <- cross_recurrence(x, y)
  expect_equal(dim(fit$matrix), c(30, 30))
  expect_true(all(is.finite(fit$summary$recurrence_rate)))
})
