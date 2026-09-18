test_that("accuracy and precision are not interchangeable", {
  d <- data.frame(timestamp_ms = 0:5 * 10, gaze_x = 0, gaze_y = 0, target_x = 0, target_y = 0)
  expect_equal(compute_gaze_accuracy(d)$accuracy_mean, 0)
  expect_equal(compute_rms_s2s(d, time = "timestamp_ms")$precision_rms_s2s, 0)
  d$gaze_x <- 1
  expect_equal(compute_gaze_accuracy(d)$accuracy_mean, 1)
  expect_equal(compute_rms_s2s(d, time = "timestamp_ms")$precision_rms_s2s, 0)
})

test_that("RMS-S2S does not bridge missing samples", {
  d <- data.frame(timestamp_ms = c(0, 10, 20), gaze_x = c(0, NA, 10), gaze_y = c(0, NA, 0))
  expect_true(is.na(compute_rms_s2s(d, time = "timestamp_ms")$precision_rms_s2s))
})

test_that("BCEA exposes probability and analytical formula", {
  set.seed(1); x <- rnorm(500); y <- .8 * x + rnorm(500, sd = .3); d <- data.frame(gaze_x = x, gaze_y = y)
  out <- compute_bcea(d, probability = .68)
  sd_pop <- function(z) sqrt(mean((z - mean(z))^2))
  expected <- 2 * pi * (-log(.32)) * sd_pop(x) * sd_pop(y) * sqrt(1 - cor(x, y)^2)
  expect_equal(out$bcea, expected, tolerance = 1e-12)
  expect_equal(out$bcea_probability, .68)
})

test_that("timestamp issues are localized to affected groups", {
  d <- data.frame(grp = rep(c("a", "b"), each = 4), timestamp_ms = c(0,10,20,30,0,10,10,5), gaze_x = 0, gaze_y = 0, target_x = 0, target_y = 0)
  v <- validate_gaze_quality_inputs(d, time = "timestamp_ms", target_x = "target_x", target_y = "target_y", by = "grp")
  expect_equal(nrow(v$group_issues), 1)
  expect_equal(v$group_issues$grp, "b")
  expect_match(v$group_issues$issues, "duplicate_timestamps")
  expect_match(v$group_issues$issues, "non_monotonic_timestamps")
})

test_that("data loss distinguishes missing and invalid samples", {
  d <- data.frame(timestamp_ms = c(0,10,20,30), gaze_x = c(0,NA,1,1), gaze_y = c(0,NA,1,1), valid = c(1,1,0,1), missing_reason = c(NA,"blink",NA,NA))
  q <- compute_gaze_data_loss(d, valid = "valid", missing_reason = "missing_reason")
  expect_equal(q$valid_sample_fraction, .5)
  expect_equal(q$data_loss_fraction, .5)
  expect_equal(q$missing_run_count, 1)
  expect_equal(q$missing_reason_blink_fraction, .5)
})

test_that("unit conversion is explicit", {
  geom <- list(screen_width_px = 1920, screen_height_px = 1080, screen_width_cm = 53, screen_height_cm = 29.8, viewing_distance_cm = 60)
  d <- data.frame(gaze_x = c(960,1060), gaze_y = c(540,540), target_x = c(960,960), target_y = c(540,540))
  expect_equal(compute_gaze_accuracy(d, unit = "pixels")$unit, "px")
  expect_equal(compute_gaze_accuracy(d, unit = "pixels", output_unit = "degrees", geometry = geom)$unit, "deg")
  expect_error(compute_gaze_accuracy(d, unit = "pixels", output_unit = "degrees"), "geometry")
})

test_that("thresholds flag review and never exclude", {
  d <- data.frame(grp = "a", timestamp_ms = c(0,10,20,30), gaze_x = 1, gaze_y = 0, target_x = 0, target_y = 0)
  r <- create_gaze_quality_report(d, by = "grp", thresholds = list(accuracy_mean = list(max = .5)))
  expect_false(anyDuplicated(names(r)) > 0L)
  expect_equal(r$accuracy_unit, "deg")
  expect_equal(r$precision_rms_s2s_unit, "deg")
  expect_equal(r$precision_sd_unit, "deg")
  expect_equal(r$bcea_unit, "deg^2")
  expect_true(r$review_required)
  expect_match(r$quality_flags, "accuracy_mean>max", fixed = TRUE)
  expect_false(attr(r, "gaze_quality_provenance")$automatic_exclusion)
})

test_that("synthetic profiles distinguish quality dimensions", {
  d <- simulate_gaze_quality_calibration(samples_per_target = 8)
  expect_equal(length(unique(d$profile)), 6)
  expect_equal(length(unique(d$target_id)), 9)
  r <- create_gaze_quality_report(d, by = c("profile", "target_id"), valid = "valid", missing_reason = "missing_reason", nominal_sampling_hz = 60)
  means <- aggregate(cbind(accuracy_mean, precision_rms_s2s, data_loss_fraction, effective_sampling_hz) ~ profile, r, mean, na.rm = TRUE)
  row <- function(p) means[means$profile == p, ]
  expect_gt(row("poor_accuracy_good_precision")$accuracy_mean, row("good_accuracy_good_precision")$accuracy_mean)
  expect_gt(row("good_accuracy_poor_precision")$precision_rms_s2s, row("good_accuracy_good_precision")$precision_rms_s2s)
  expect_gt(row("missingness")$data_loss_fraction, 0)
})

test_that("R and Python share the frozen spatial-quality fixture", {
  input <- utils::read.csv(testthat::test_path("fixtures", "spatial_quality_input.csv"), stringsAsFactors = FALSE)
  expected <- utils::read.csv(testthat::test_path("fixtures", "spatial_quality_expected.csv"), stringsAsFactors = FALSE)
  actual <- create_gaze_quality_report(input, by = "fixture_group", valid = "valid", nominal_sampling_hz = 100, bcea_probability = .68)
  for (nm in names(expected)) {
    if (nm == "fixture_group") expect_equal(as.character(actual[[nm]]), as.character(expected[[nm]])) else
      expect_equal(as.numeric(actual[[nm]]), as.numeric(expected[[nm]]), tolerance = 1e-10)
  }
})

test_that("canonical quality report honors explicit unit metadata and short-group flags", {
  d <- data.frame(
    timestamp_ms = c(0, 10),
    gaze_x = c(0, 0),
    gaze_y = c(0, 0),
    target_x = c(0, 0),
    target_y = c(0, 0),
    coordinate_unit = c("degrees", "degrees")
  )
  bad <- d
  bad$coordinate_unit[2] <- "pixels"
  expect_error(create_gaze_quality_report(bad), "mixed coordinate units")

  one <- data.frame(timestamp_ms = 0, gaze_x = 1, gaze_y = 2)
  q <- create_gaze_quality_report(one, target_x = NULL, target_y = NULL)
  expect_match(q$quality_flags, "insufficient_bcea_samples")
  expect_match(q$quality_flags, "insufficient_rms_pairs")
})

test_that("quality provenance fingerprint includes validity decisions", {
  d <- data.frame(
    timestamp_ms = c(0, 10, 20),
    gaze_x = c(0, 0, 0),
    gaze_y = c(0, 0, 0),
    target_x = 0,
    target_y = 0,
    valid = c(1, 1, 1)
  )
  a <- create_gaze_quality_report(d, valid = "valid")
  d$valid[1] <- 0
  b <- create_gaze_quality_report(d, valid = "valid")
  expect_false(identical(
    attr(a, "gaze_quality_provenance")$source_fingerprint,
    attr(b, "gaze_quality_provenance")$source_fingerprint
  ))
})
