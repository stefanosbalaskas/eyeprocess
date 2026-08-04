test_that("trials, AOIs, visits, and features form a common workflow", {
  x <- read_gazepoint(extdata("gazepoint", "demo-user.csv"), recording_id = "R1", quiet = TRUE)
  x <- build_trials(x, start_events = "TRIAL_START", end_events = "TRIAL_END")
  expect_true(sum(x$intervals$interval_type == "trial") >= 2)
  x <- register_aois(x,
    new_aoi("left", x = 0, y = 0, width = .5, height = 1),
    new_aoi("right", x = .5, y = 0, width = .5, height = 1)
  )
  x <- assign_aois(x)
  x <- build_aoi_visits(x)
  x <- derive_all_features(x)
  expect_true(nrow(x$features) > 0)
  expect_true(all(c("feature_name", "level", "method") %in% names(x$features)))
})
