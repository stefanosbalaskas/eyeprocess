test_that("canonical schemas and datasets are internally coherent", {
  expect_true(all(c("recordings", "gaze_samples", "provenance") %in% canonical_table_names()))
  x <- new_eye_dataset()
  expect_s3_class(x, "eye_dataset")
  expect_equal(nrow(x$recordings), 0)
  expect_s3_class(validate_eye_dataset(x), "eye_validation")
  expect_equal(nrow(validate_eye_dataset(x)), 0)
})

test_that("coordinate spaces retain explicit semantics", {
  c <- new_coordinate_space("screen", "display_pixels_top_left", width = 1920, height = 1080)
  expect_equal(c$origin, "top_left")
  expect_equal(c$x_unit, "pixels")
})


test_that("zero-row canonical tables can be standardized safely", {
  x <- data.frame(extra = logical(0))
  out <- standardize_eye_table(x, "streams")
  expect_equal(nrow(out), 0)
  expect_true(all(schema_table("streams") %in% names(out)))
})
