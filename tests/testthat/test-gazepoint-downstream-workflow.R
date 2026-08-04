test_that("integrated Gazepoint workflow creates complete downstream outputs", {
  source_dir <- system.file("extdata", "gazepoint_v72", package = "eyeprocess")
  expect_true(dir.exists(source_dir))

  output_dir <- tempfile("eyeprocess-workflow-")
  spec <- gazepoint_workflow_spec(
    create_plots = FALSE,
    create_html_report = FALSE,
    retain_raw = FALSE,
    pupil_baseline = "none"
  )

  result <- run_gazepoint_workflow(
    source_dir,
    output_dir = output_dir,
    spec = spec,
    overwrite = TRUE,
    quiet = TRUE
  )

  expect_s3_class(result, "eye_gazepoint_workflow")
  expect_equal(result$status, "pass")
  expect_equal(nrow(result$dataset$recordings), 1L)
  expect_equal(nrow(result$tables$trials), 2L)
  expect_equal(nrow(result$tables$process), 2L)
  expect_equal(result$irt$status, "process_ready_response_pending")
  expect_true(nrow(result$tables$fixation_summary) > 0L)
  expect_true(nrow(result$tables$pupil_summary) > 0L)
  expect_true(nrow(result$tables$biometric_summary) > 0L)
  expect_true(file.exists(result$paths$report))
  expect_true(dir.exists(result$paths$canonical_dataset))
  expect_true(file.exists(file.path(output_dir, "irt", "response-template.csv")))
  expect_true(file.exists(file.path(output_dir, "tables", "process.csv")))
  expect_true(file.exists(file.path(output_dir, "rerun-workflow.R")))

  checks <- validate_gazepoint_workflow(result)
  expect_true(all(checks$passed))
})

test_that("workflow accepts observed responses without inventing scores", {
  source_dir <- system.file("extdata", "gazepoint_v72", package = "eyeprocess")
  output_dir <- tempfile("eyeprocess-workflow-responses-")
  responses <- data.frame(
    participant_id = c("User 3", "User 3"),
    item_id = c("0", "1"),
    response = c("yes", "no"),
    response_time = c(0.8, 0.7),
    stringsAsFactors = FALSE
  )

  result <- run_gazepoint_workflow(
    source_dir,
    output_dir = output_dir,
    responses = responses,
    score_key = c("0" = "yes", "1" = "no"),
    spec = gazepoint_workflow_spec(
      create_plots = FALSE,
      create_html_report = FALSE,
      retain_raw = FALSE
    ),
    overwrite = TRUE,
    quiet = TRUE
  )

  expect_true(result$responses_supplied)
  expect_equal(sum(result$dataset$responses$score), 2)
  expect_equal(result$irt$status, "structurally_ready_validation_only")
  expect_false(is.null(result$irt$response_matrix))
  expect_equal(dim(result$irt$response_matrix), c(1L, 2L))
})

test_that("workflow wide features combine heterogeneous trial schemas", {
  features <- data.frame(
    recording_id = c("R1", "R2"),
    participant_id = c("P1", "P2"),
    trial_id = c("T1", "T2"),
    item_id = c("I1", "I2"),
    stimulus_id = c("S1", "S2"),
    feature_name = c("fixation_count", "heart_rate_mean"),
    value = c(4, 72),
    stringsAsFactors = FALSE
  )

  result <- eyeprocess:::.workflow_wide_features(
    features,
    c(
      "recording_id", "participant_id", "trial_id",
      "item_id", "stimulus_id"
    )
  )

  expect_equal(nrow(result), 2L)
  expect_true(all(c(
    "fixation_count", "heart_rate_mean"
  ) %in% names(result)))

  expect_equal(
    result$fixation_count[result$trial_id == "T1"],
    4
  )

  expect_true(is.na(
    result$heart_rate_mean[result$trial_id == "T1"]
  ))

  expect_true(is.na(
    result$fixation_count[result$trial_id == "T2"]
  ))

  expect_equal(
    result$heart_rate_mean[result$trial_id == "T2"],
    72
  )
})
