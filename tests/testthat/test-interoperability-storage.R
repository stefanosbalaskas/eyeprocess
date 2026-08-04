test_that("RDS storage round trips an eye dataset", {
  x <- simulate_eye_dataset(n_person = 4, n_item = 3, sampling_rate = 10, trial_duration = .3, seed = 101)
  path <- tempfile(fileext = ".rds")
  handle <- write_eye_storage(x, path, format = "rds")
  expect_s3_class(handle, "eye_storage")
  y <- collect_eye_storage(handle)
  expect_s3_class(y, "eye_dataset")
  expect_equal(nrow(y$responses), nrow(x$responses))
})

test_that("Eye-Tracking-BIDS export and import preserve core gaze structure", {
  skip_if_not_installed("jsonlite")
  x <- simulate_eye_dataset(n_person = 2, n_item = 2, sampling_rate = 10, trial_duration = .3, seed = 102)
  path <- tempfile("bids-")
  manifest <- export_eye_bids(x, path, task = "demo", overwrite = TRUE)
  expect_true(nrow(manifest) >= 2)
  expect_equal(anyDuplicated(manifest$tsv), 0L)
  expect_true(file.exists(file.path(path, "dataset_description.json")))
  sidecar <- jsonlite::read_json(manifest$json[[1L]], simplifyVector = TRUE)
  expect_true(all(c("Columns", "PhysioType", "StartTime", "RecordedEye", "SamplingFrequency") %in% names(sidecar)))
  expect_true(sidecar$RecordedEye %in% c("left", "right", "cyclopean"))
  expect_true(is.finite(as.numeric(sidecar$StartTime)))
  y <- import_eye_bids(path)
  expect_s3_class(y, "eye_dataset")
  expect_gt(nrow(y$gaze_samples), 0)
  expect_equal(nrow(y$gaze_samples), nrow(x$gaze_samples))
  expect_equal(anyDuplicated(y$gaze_samples$sample_id), 0L)
  if (nrow(y$eye_samples)) expect_true(all(y$eye_samples$sample_id %in% y$gaze_samples$sample_id))
  expect_gt(nrow(y$events), 0)
  expect_equal(length(unique(y$recordings$participant_id)), 2)
  expect_true(all(is.finite(y$gaze_samples$timestamp_seconds)))
  if (nrow(y$eye_samples)) expect_true(any(is.finite(y$eye_samples$pupil_diameter)))
})

test_that("external and sequence adapters return declared contracts", {
  d <- data.frame(
    participant = "P1", recording = "R1", timestamp = 1:4,
    x = c(.1, .2, .3, .4), y = c(.2, .3, .4, .5), trial = "T1"
  )
  mapping <- eye_mapping(participant = "participant", recording = "recording", timestamp = "timestamp", x = "x", y = "y", trial = "trial")
  adapters <- list(
    as_eyeprocess_eyetools,
    as_eyeprocess_eyetrackingr,
    as_eyeprocess_gazer,
    as_eyeprocess_eyeris,
    as_eyeprocess_pupillometryr
  )
  converted <- lapply(adapters, function(adapter) {
    adapter(d, mapping = mapping, time_unit = "seconds", coordinate_space = "display_normalized_top_left")
  })
  expect_true(all(vapply(converted, inherits, logical(1), "eye_dataset")))
  x <- converted[[1L]]
  sim <- simulate_eye_dataset(n_person = 2, n_item = 2, sampling_rate = 10, trial_duration = .3, seed = 103)
  proc <- as_procdata_sequence(sim, source = "samples")
  expect_true(all(c("recording_id", "trial_id", "action") %in% names(proc)))
  traminer <- as_traminer_sequence(sim, source = "samples")
  expect_s3_class(traminer, "eye_traminer_sequence")
  hmm <- as_seqhmm_data(sim, source = "samples")
  expect_s3_class(hmm, "eye_seqhmm_data")
})

test_that("Parquet storage round trips when Arrow is available", {
  skip_if_not_installed("arrow")
  x <- simulate_eye_dataset(n_person = 3, n_item = 2, sampling_rate = 10, trial_duration = .3, seed = 105)
  path <- tempfile("parquet-store-")
  handle <- write_eye_storage(x, path, format = "parquet", overwrite = TRUE)
  y <- collect_eye_storage(handle)
  expect_s3_class(y, "eye_dataset")
  expect_equal(nrow(y$recordings), nrow(x$recordings))
  expect_equal(nrow(y$gaze_samples), nrow(x$gaze_samples))
  expect_equal(anyDuplicated(y$gaze_samples$sample_id), 0L)
  if (nrow(y$eye_samples)) {
    expect_equal(sort(y$eye_samples$sample_id), sort(x$eye_samples$sample_id))
  }
})

test_that("BIDS import rejects incomplete sidecars", {
  skip_if_not_installed("jsonlite")
  x <- simulate_eye_dataset(n_person = 2, n_item = 2, sampling_rate = 10, trial_duration = .3, seed = 106)
  path <- tempfile("bids-invalid-")
  manifest <- export_eye_bids(x, path, task = "demo", overwrite = TRUE)
  sidecar <- jsonlite::read_json(manifest$json[[1L]], simplifyVector = TRUE)
  sidecar$StartTime <- NULL
  jsonlite::write_json(sidecar, manifest$json[[1L]], auto_unbox = TRUE, pretty = TRUE)
  expect_error(import_eye_bids(path), "required fields")
})

test_that("partitioned Arrow dataset storage round trips when Arrow is available", {
  skip_if_not_installed("arrow")
  x <- simulate_eye_dataset(n_person = 3, n_item = 2, sampling_rate = 10, trial_duration = .3, seed = 107)
  path <- tempfile("arrow-dataset-store-")
  handle <- write_eye_storage(
    x, path, format = "arrow_dataset",
    partitioning = "recording_id", overwrite = TRUE
  )
  y <- collect_eye_storage(handle)
  expect_s3_class(y, "eye_dataset")
  expect_equal(nrow(y$recordings), nrow(x$recordings))
  expect_equal(nrow(y$responses), nrow(x$responses))
})


test_that("optional psychometric adapters validate contracts before engine loading", {
  x <- simulate_eye_dataset(n_person = 4, n_item = 3, sampling_rate = 10, trial_duration = .3, seed = 108)
  expect_error(fit_gdina_adapter(x, matrix(1, nrow = 1, ncol = 1)), "one row per")
  expect_error(fit_diffirt_adapter(x, model = "unsupported"), "arg")
  expect_error(fit_openmx_process_model(x, model_builder = NULL), "must be a function")
})
