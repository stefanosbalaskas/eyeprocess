test_that("process episodes detect, segment, and label", {
  set.seed(1)
  data <- data.frame(time = 1:120, pupil = c(rnorm(40, 0), rnorm(40, 2), rnorm(40, -1)), gaze_velocity = c(rnorm(40, 1), rnorm(40, 3), rnorm(40, 0.5)))
  changes <- detect_process_changepoints(data, c("pupil", "gaze_velocity"), time_col = "time", window = 8)
  expect_s3_class(changes, "eye_process_changepoints")
  episodes <- label_process_episodes(segment_process_episodes(changes))
  expect_true("episode_label" %in% names(episodes$summary))
  comparison <- compare_episode_structure(episodes, rep(c("A", "B"), length.out = nrow(episodes$data)))
  expect_s3_class(comparison, "eye_episode_comparison")
  expect_plot_silent(plot_process_episodes(episodes))
})

test_that("change-point window validation handles the minimum valid length", {
  data <- data.frame(signal = c(rep(0, 10), rep(1, 11)))
  fit <- detect_process_changepoints(data, channels = "signal", window = 10)
  expect_s3_class(fit, "eye_process_changepoints")
  expect_length(fit$score, 21)
})
