test_that("API and object schemas are explicit", {
  expect_equal(as.character(eyeprocess_api_version()), "0.5.0")
  schema <- object_schema("eye_storage")
  expect_equal(schema$version, "2.0.0")
  expect_match(schema$invariant, "atomic")
})

test_that("partitioned RDS storage round trips and detects corruption", {
  x <- list(
    responses = data.frame(participant_id = rep(c("P1", "P2"), each = 2), recording_id = rep(c("R1", "R2"), each = 2), score = c(1,0,1,1)),
    gaze_samples = data.frame(participant_id = rep(c("P1", "P2"), each = 3), recording_id = rep(c("R1", "R2"), each = 3), sample_id = 1:6, x = seq(.1,.6,.1))
  )
  path <- tempfile("partitioned-storage-")
  storage <- write_partitioned_eye_storage(x, path, partition_eye_storage(by = c("participant_id", "recording_id"), format = "rds", max_rows = 2L))
  expect_s3_class(storage, "eye_partitioned_storage")
  result <- query_eye_storage(storage, "responses", filters = list(participant_id = "P1"))
  expect_equal(nrow(result), 2L)
  validation <- validate_eye_storage_metadata(storage)
  expect_true(validation$valid)
  expect_equal(nrow(detect_corrupt_partitions(storage)), 0L)
})

test_that("storage migration retains rows", {
  x <- list(responses = data.frame(participant_id = c("P1","P2"), recording_id = c("R1","R2"), score = c(1,0)))
  source <- write_partitioned_eye_storage(x, tempfile("storage-source-"), partition_eye_storage(format = "rds"))
  target <- migrate_eye_storage_schema(source, tempfile("storage-target-"), format = "csv")
  expect_equal(nrow(query_eye_storage(target, "responses")), 2L)
  expect_equal(target$metadata$schema_version, "2.0.0")
})

test_that("optional adapters return explicit availability states", {
  status <- external_model_engines()
  expect_true(all(c("engine", "package", "available") %in% names(status)))
  result <- fit_external_engine("mirt", matrix(c(1,0,1,1), 2), purpose = "contract test")
  expect_s3_class(result, "eye_engine_adapter_result")
  expect_true(result$status %in% c("fitted", "not_available", "failed"))
  contract <- validate_engine_adapter(result)
  expect_true(contract$valid)
})
