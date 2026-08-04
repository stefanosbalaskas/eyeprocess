test_that("dedicated vendor adapters import fixtures", {
  tobii <- read_tobii(extdata("tobii-demo.tsv"), recording_id = "T1", quiet = TRUE)
  expect_equal(nrow(tobii$gaze_samples), 3)
  expect_equal(tobii$recordings$vendor, "Tobii")

  neon <- read_pupil_neon(extdata("pupillabs_neon"), recording_id = "N1", quiet = TRUE)
  expect_equal(nrow(neon$gaze_samples), 3)
  expect_true(nrow(neon$eye_samples) >= 6)

  core <- read_pupil_core(extdata("pupillabs_core"), recording_id = "C1", quiet = TRUE)
  expect_equal(nrow(core$gaze_samples), 3)
  expect_equal(core$coordinate_spaces$origin, "bottom_left")

  eyelink <- read_eyelink_asc(extdata("eyelink-demo.asc"), recording_id = "E1", quiet = TRUE)
  expect_equal(nrow(eyelink$gaze_samples), 3)
  expect_true(any(eyelink$episodes$episode_type == "fixation"))

  smi <- read_smi(extdata("smi-demo.txt"), recording_id = "S1", quiet = TRUE)
  expect_equal(nrow(smi$gaze_samples), 3)
})
