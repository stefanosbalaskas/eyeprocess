test_that("bundled benchmark contains all multimodal layers", {
  study <- eyeprocess_benchmark_study()
  expect_s3_class(study, "eye_benchmark_study")
  required <- c("participants", "items", "responses", "gaze_samples", "events", "aoi_definitions", "pupil_samples", "quality", "provenance")
  expect_true(all(required %in% study$manifest$table))
})

test_that("benchmark fingerprints and relations validate", {
  validation <- validate_benchmark_study()
  expect_s3_class(validation, "eye_benchmark_validation")
  expect_true(validation$valid)
  expect_true(all(validation$relations$passed))
})


test_that("benchmark logical columns use stable R types", {
  gaze <- read_benchmark_table(eyeprocess_benchmark_study(), "gaze_samples")
  pupil <- read_benchmark_table(eyeprocess_benchmark_study(), "pupil_samples")

  expect_type(gaze$valid, "logical")
  expect_type(pupil$blink, "logical")
  expect_type(pupil$valid, "logical")
  expect_false(anyNA(gaze$valid))
  expect_false(anyNA(pupil$blink))
  expect_false(anyNA(pupil$valid))
  expect_equal(mean(gaze$valid), 0.96375, tolerance = 1e-10)
})

test_that("benchmark expected outputs reproduce exactly", {
  result <- run_benchmark_reproduction()
  expect_s3_class(result, "eye_benchmark_reproduction")
  expect_true(result$passed)
  expect_true(all(result$comparison$passed))
})

test_that("software-paper scaffold is self-contained", {
  directory <- tempfile("software-paper-")
  manifest <- write_software_paper_reproduction(directory)
  expect_true(file.exists(file.path(directory, "scripts", "run-reproduction.R")))
  expect_true(file.exists(file.path(directory, "README.md")))
  expect_s3_class(manifest, "eye_reproducibility_manifest")
  expect_true(all(verify_reproducibility_manifest(manifest)$unchanged))
})
