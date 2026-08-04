test_that("Gazepoint Analysis 7.2 real export names are classified correctly", {
  root <- system.file("extdata", "gazepoint_v72", package = "eyeprocess")
  gaze <- file.path(root, "user3_all_gaze.csv")
  fix <- file.path(root, "user3_fixations.csv")
  summary <- file.path(root, "Data_Summary_export_02-20-26-01.28.43.csv")

  expect_equal(gp_identify_export_type(gaze), "combined_biometrics")
  expect_equal(gp_identify_export_type(fix), "fixations")
  expect_equal(gp_identify_export_type(summary), "aoi_statistics")
  expect_gte(is_gazepoint_export(gaze), 0.9)
  expect_gte(is_gazepoint_export(fix), 0.9)
  expect_gte(is_gazepoint_export(summary), 0.9)
})

test_that("Gazepoint TIMETICK is normalized while media time is retained", {
  root <- system.file("extdata", "gazepoint_v72", package = "eyeprocess")
  x <- read_gazepoint(file.path(root, "user3_all_gaze.csv"), quiet = TRUE)

  expect_s3_class(x, "eye_dataset")
  expect_equal(unique(x$recordings$participant_id), "User 3")
  expect_true(all(diff(x$gaze_samples$timestamp_seconds) >= 0))
  expect_true("media_time_seconds" %in% names(x$gaze_samples))
  expect_true(any(diff(x$gaze_samples$media_time_seconds) < 0))
  expect_true(all(is.finite(x$gaze_samples$timestamp_native)))
  expect_equal(unique(x$streams$timestamp_unit), "ticks")
  expect_true(any(x$streams$source_clock == "Gazepoint TIMETICK"))
  expect_true(all(c(
    "gsr_raw", "eda", "skin_conductance_level",
    "skin_conductance_response", "heart_rate",
    "interbeat_interval", "engagement_dial"
  ) %in% unique(x$biometrics$channel)))
  expect_equal(unique(x$biometrics$unit[x$biometrics$channel == "interbeat_interval"]), "seconds")
})

test_that("Gazepoint fixation IDs are namespaced by media", {
  root <- system.file("extdata", "gazepoint_v72", package = "eyeprocess")
  gaze <- read_gazepoint(file.path(root, "user3_all_gaze.csv"), quiet = TRUE)
  origin <- gaze$vendor_metadata$gazepoint_timebase$origin_tick
  fix <- read_gazepoint_fixations(
    file.path(root, "user3_fixations.csv"),
    participant_id = "User 3",
    recording_id = gaze$recordings$recording_id[1L],
    origin_tick = origin,
    quiet = TRUE
  )

  expect_false(anyDuplicated(fix$episodes$episode_id) > 0L)
  expect_true(all(fix$episodes$end_time >= fix$episodes$start_time))
  expect_equal(length(unique(fix$episodes$stimulus_id)), 2L)
  expect_true(all(grepl("media_", stats::na.omit(fix$episodes$aoi_id))))
})

test_that("Gazepoint Data Summary exports become AOI definitions and features", {
  root <- system.file("extdata", "gazepoint_v72", package = "eyeprocess")
  path <- file.path(root, "Data_Summary_export_02-20-26-01.28.43.csv")
  report <- read_gazepoint_summary(path)
  expect_s3_class(report, "gazepoint_summary")
  expect_equal(report$software_version, "v7.2.0")
  expect_gt(nrow(report$aoi_summary), 0L)
  expect_gt(nrow(report$aoi_statistics), 0L)

  x <- read_gazepoint_aoi_statistics(path, quiet = TRUE)
  expect_gt(nrow(x$aoi_definitions), 0L)
  expect_gt(nrow(x$features), 0L)
  expect_false(anyDuplicated(x$features$feature_id) > 0L)
  expect_true(all(c("aoi_viewed", "time_viewed", "mean_heart_rate") %in% unique(x$features$feature_name)))
})

test_that("a genuine Gazepoint 7.2 folder imports as one paired recording", {
  root <- system.file("extdata", "gazepoint_v72", package = "eyeprocess")
  pairs <- gp_pair_exports(root)
  expect_true(all(c("combined_biometrics", "fixations", "aoi_statistics") %in% pairs$export_type))

  x <- read_gazepoint_folder(root, keep_raw = TRUE, quiet = TRUE)
  expect_s3_class(x, "eye_dataset")
  expect_equal(nrow(x$recordings), 1L)
  expect_equal(unique(x$recordings$participant_id), "User 3")
  expect_gt(nrow(x$gaze_samples), 0L)
  expect_gt(nrow(x$episodes), 0L)
  expect_gt(nrow(x$features), 0L)
  expect_false(anyDuplicated(x$episodes$episode_id) > 0L)

  result <- validate_eye_source(
    root,
    vendor = "gazepoint",
    spec = format_validation_spec(require_raw_retention = TRUE, run_roundtrip = TRUE),
    case_id = "gazepoint-v72-real-structure"
  )
  expect_true(result$status %in% c("pass", "warning"))
})
