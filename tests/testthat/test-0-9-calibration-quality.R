test_that("0.9 calibration uncertainty and quality metrics work", {
  set.seed(9)
  d <- data.frame(target_x=rep(c(.2,.5,.8), each=10), target_y=rep(c(.2,.5,.8), each=10),
                  gaze_x=rep(c(.2,.5,.8), each=10)+rnorm(30,0,.01), gaze_y=rep(c(.2,.5,.8), each=10)+rnorm(30,0,.01))
  m <- calibration_error_model(d)
  expect_s3_class(m, "eye_calibration_error_model")
  expect_equal(nrow(gaze_uncertainty_ellipse(m)), 1)
  g <- data.frame(timestamp_ms=seq(0,990,by=10), gaze_x=rnorm(100), gaze_y=rnorm(100), valid=TRUE)
  q <- gaze_data_quality_profile(g, valid="valid")
  expect_s3_class(q, "eye_data_quality_profile")
  expect_true(is.finite(q$table$effective_hz))
  g$valid[1] <- NA
  q2 <- gaze_data_quality_profile(g, valid="valid")
  expect_true(is.finite(q2$table$valid_fraction))
  aois <- data.frame(aoi="A", x_min=0, x_max=1, y_min=0, y_max=1)
  bd <- fixation_boundary_uncertainty(data.frame(gaze_x=c(0.5, NA), gaze_y=c(0.5, 0.2)), aois)
  expect_true(is.na(bd$signed_boundary_distance[2]))
})
