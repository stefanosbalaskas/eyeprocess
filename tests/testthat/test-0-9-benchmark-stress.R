test_that("0.9 corruption plans are explicit and reproducible", {
  d <- data.frame(gaze_x=1:100/100, gaze_y=1:100/100, pupil=3, timestamp_ms=1:100, aoi=rep(c("A","B"),50))
  p <- synthetic_corruption_plan(missingness=.1, pupil_dropout=.1, seed=9)
  a <- apply_synthetic_corruption(d, p, aoi="aoi")
  b <- apply_synthetic_corruption(d, p, aoi="aoi")
  expect_identical(a, b)
  expect_true(sum(is.na(a$gaze_x)) > 0)
  expect_true(sum(is.na(a$pupil)) > 0)
})
