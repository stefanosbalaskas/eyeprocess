test_that("Gazepoint sample and biometric fields are imported", {
  p <- extdata("gazepoint", "demo-user.csv")
  expect_gt(is_gazepoint_export(p), 0.8)
  x <- read_gazepoint(p, recording_id = "R1", quiet = TRUE)
  expect_equal(nrow(x$gaze_samples), 12)
  expect_equal(nrow(x$eye_samples), 24)
  expect_true(all(c("heart_rate", "gsr_raw") %in% unique(x$biometrics$channel)))
  expect_false("eda" %in% unique(x$biometrics$channel))
  expect_true(any(x$events$event_name == "TRIAL_START item01"))
})

test_that("Gazepoint fixation exports retain vendor derivation", {
  x <- read_gazepoint_fixations(extdata("gazepoint", "demo-user-fix.csv"), recording_id = "R1", quiet = TRUE)
  expect_equal(nrow(x$episodes), 4)
  expect_true(all(x$episodes$derived_by == "vendor"))
})

test_that("Gazepoint folder import combines identity tables safely", {
  x <- read_gazepoint_folder(extdata("gazepoint"), recording_id = "R1", quiet = TRUE)
  expect_s3_class(x, "eye_dataset")
  expect_equal(unique(x$recordings$recording_id), "R1")
  expect_false(anyDuplicated(x$recordings$recording_id) > 0)

  via_generic <- read_gazepoint(extdata("gazepoint"), recording_id = "R2", quiet = TRUE)
  expect_equal(unique(via_generic$recordings$recording_id), "R2")
})

test_that("Gazepoint biometric matching uses explicit columns", {
  src <- extdata("gazepoint")
  tmp <- tempfile("eyeprocess-gp-")
  dir.create(tmp)
  files <- list.files(src, full.names = TRUE)
  file.copy(files, tmp, overwrite = TRUE)

  matched <- gp_match_biometrics(tmp)
  expect_s3_class(matched, "data.frame")
  expect_true(all(matched$export_type %in% c("combined_biometrics", "gaze")))
})
