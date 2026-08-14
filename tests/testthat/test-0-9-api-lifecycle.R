test_that("0.9 API lifecycle is explicit and non-destructive", {
  reg <- eye_api_lifecycle()
  reg <- register_eye_api_status(reg, "run_eye_pipeline", "workflow", canonical = "run_eye_pipeline")
  expect_equal(eye_api_status("run_eye_pipeline", reg)$status, "workflow")
  expect_equal(eye_api_status("unknown_symbol", reg)$status, "unreviewed")
  expect_true("run_eye_pipeline" %in% canonical_eye_api(reg)$name)
  expect_true(validate_process_measure_registry(process_measure_registry()))
})

test_that("0.9 API audits honor registry canonical and replacement mappings", {
  inv <- data.frame(
    name = "run_eye_pipeline",
    status = "unreviewed",
    canonical = NA_character_,
    replacement = NA_character_,
    stringsAsFactors = FALSE
  )
  reg <- eye_api_lifecycle()
  reg <- register_eye_api_status(
    reg,
    "run_eye_pipeline",
    "workflow",
    canonical = "missing_canonical_api"
  )
  audit <- audit_eye_api(inv, reg)
  expect_false(audit$valid)
  expect_equal(audit$invalid_canonical, "run_eye_pipeline")
  expect_equal(audit$table$status, "workflow")
  expect_equal(audit$table$canonical, "missing_canonical_api")
})

# BEGIN EYEPROCESS 0.9 API LIFECYCLE CLOSURE
test_that("packaged lifecycle registry covers the current public API", {
  reg <- eye_api_lifecycle()
  exports <- sort(getNamespaceExports("eyeprocess"))

  expect_equal(nrow(reg), length(exports))
  expect_setequal(reg$name, exports)
  expect_false(anyDuplicated(reg$name) > 0L)
  expect_false(any(reg$status == "unreviewed"))
  expect_true(all(reg$status %in% c(
    "core", "workflow", "advanced", "experimental", "gated",
    "compatibility", "deprecated", "internal-candidate"
  )))
})

test_that("default inventory and audit use the packaged lifecycle registry", {
  inv <- eye_api_inventory()
  reg <- eye_api_lifecycle()
  audit <- audit_eye_api(inv, reg)

  expect_false(any(inv$status == "unreviewed"))
  expect_equal(audit$reviewed_fraction, 1)
  expect_length(audit$unreviewed, 0L)
  expect_true(audit$valid)
  expect_length(audit$invalid_replacements, 0L)
  expect_length(audit$invalid_canonical, 0L)
})

test_that("future exports still fall back to unreviewed", {
  reg <- eye_api_lifecycle()
  future <- eye_api_status("__eyeprocess_future_export__", reg)
  expect_equal(future$status, "unreviewed")
})

test_that("packaged lifecycle policy artifacts are internally coherent", {
  registry_path <- system.file("extdata", "api-lifecycle-registry-0.9.csv", package = "eyeprocess")
  policy_path <- system.file("extdata", "api-lifecycle-module-policy-0.9.csv", package = "eyeprocess")

  expect_true(nzchar(registry_path))
  expect_true(file.exists(registry_path))
  expect_true(nzchar(policy_path))
  expect_true(file.exists(policy_path))

  reg <- utils::read.csv(registry_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
  policy <- utils::read.csv(policy_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))

  expect_equal(nrow(reg), 1149L)
  expect_equal(nrow(policy), 96L)
  expect_false(anyDuplicated(reg$name) > 0L)
  expect_false(anyDuplicated(policy$source_file) > 0L)
})
# END EYEPROCESS 0.9 API LIFECYCLE CLOSURE
