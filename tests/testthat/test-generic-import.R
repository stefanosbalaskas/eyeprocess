test_that("generic mappings produce canonical tables", {
  d <- data.frame(
    id = "P1", rec = "R1", t = c(0, 0.01, 0.02),
    gx = c(.1, .2, .3), gy = c(.4, .5, .6), pupil = c(3.1, 3.2, 3.3)
  )
  m <- eye_mapping(participant = "id", recording = "rec", timestamp = "t", x = "gx", y = "gy", pupil_left = "pupil")
  x <- read_eye_generic(d, mapping = m, pupil_unit = "millimetres", quiet = TRUE)
  expect_s3_class(x, "eye_dataset")
  expect_equal(nrow(x$gaze_samples), 3)
  expect_equal(nrow(x$eye_samples), 3)
  expect_equal(x$recordings$vendor, "generic")
  expect_true(nrow(x$provenance) >= 1)
})
