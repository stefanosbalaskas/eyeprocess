test_that("semantic fidelity distinguishes lossless and affine transforms", {
  a <- data.frame(id = 1:5, timestamp = 1:5, x = c(10, 20, 30, 40, 50),
                  y = c(5, 6, 7, 8, 9), pupil_size = c(3, 3.1, 3.2, 3.3, 3.4))
  b <- a
  rep <- field_fidelity_report(a, b, key = "id")
  expect_s3_class(rep, "eye_field_fidelity_report")
  expect_true(is.data.frame(rep$fields))
  keep <- rep$fields$field %in% c("timestamp", "x", "y", "pupil_size")
  expect_equal(sum(keep), 4L)
  expect_true(all(rep$fields$status[keep] == "LOSSLESS"))

  c <- b
  c$x <- c$x * 2 + 1
  rep2 <- field_fidelity_report(a, c, fields = "x", key = "id")
  expect_s3_class(rep2, "eye_field_fidelity_report")
  expect_equal(nrow(rep2$fields), 1L)
  expect_equal(rep2$fields$field, "x")
  expect_equal(rep2$fields$status, "UNIT_TRANSFORMED")
  expect_equal(rep2$fields$transform_intercept, 1, tolerance = 1e-8)
  expect_equal(rep2$fields$transform_slope, 2, tolerance = 1e-8)
})

test_that("BIDS eye semantics require key metadata", {
  d <- data.frame(timestamp = 1:3, x_coordinate = 1:3, y_coordinate = 4:6)
  meta <- list(
    Columns = c("timestamp", "x_coordinate", "y_coordinate"),
    SamplingFrequency = 60,
    PhysioType = "eyetrack",
    RecordedEye = "cyclopean",
    SampleCoordinateSystem = "eye-in-head"
  )
  z <- validate_bids_eye_semantics(d, meta)
  expect_true(is.data.frame(z$checks))
  expect_true(all(z$checks$pass))
})

test_that("public validation corpus covers four ecosystems", {
  x <- public_validation_corpus()
  expect_true(all(c("Gazepoint", "EyeLink", "Tobii", "Pupil Labs") %in% x$ecosystem))
  expect_true(any(grepl("GazeBase", x$corpus)))
  expect_true(any(grepl("MCFW", x$corpus)))
})
