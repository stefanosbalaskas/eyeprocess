test_that("0.9 API lifecycle is explicit and non-destructive", {
  reg <- eye_api_lifecycle()
  reg <- register_eye_api_status(reg, "run_eye_pipeline", "workflow", canonical = "run_eye_pipeline")
  expect_equal(eye_api_status("run_eye_pipeline", reg)$status, "workflow")
  expect_equal(eye_api_status("unknown_symbol", reg)$status, "unreviewed")
  expect_equal(nrow(canonical_eye_api(reg)), 1)
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
