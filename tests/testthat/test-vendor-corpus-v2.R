test_that("vendor cases retain version-specific evidence metadata", {
  corpus <- tempfile("vendor-corpus-")
  init_vendor_corpus(corpus)
  source <- tempfile("vendor-source-"); dir.create(source)
  write.csv(data.frame(time = 1:3, x = c(.1,.2,.3)), file.path(source, "export.csv"), row.names = FALSE)
  case <- register_validation_case(
    corpus, source, vendor = "Tobii", case_id = "tobii-demo", support_level = "fixture-tested",
    device_model = "synthetic", software_name = "Tobii Pro Lab", software_version = "test", sampling_rate_hz = 60,
    coordinate_system = "normalized", timebase = "seconds", event_semantics = "trial markers",
    ocular_structure = "binocular", missingness_convention = "NA", vendor_fixations = TRUE,
    independent_source = FALSE, licence_reviewed = TRUE
  )
  expect_s3_class(case, "eye_validation_case")
  registry <- read_vendor_registry(corpus)
  expect_equal(registry$vendor, "tobii")
  expect_equal(registry$support_level, "fixture-tested")
  fingerprint <- fingerprint_validation_case(source)
  expect_true(nzchar(fingerprint$case_fingerprint[1]))
})

test_that("compatibility matrix separates evidence levels", {
  registry <- data.frame(
    case_id = c("g1", "g2", "t1"), vendor = c("gazepoint", "gazepoint", "tobii"),
    support_level = c("empirically-validated", "empirically-validated", "fixture-tested"),
    independent_source = c(TRUE, TRUE, FALSE), licence_reviewed = TRUE,
    status = c("validated", "validated", "validated"), device_model = c("GP3","GP3","demo"),
    software_version = c("7.2", "7.2", "x"), stringsAsFactors = FALSE
  )
  matrix <- build_compatibility_matrix(registry, required_vendors = c("gazepoint", "tobii"), min_empirical_cases = 2L)
  expect_true(matrix$production_claim_allowed[matrix$vendor == "gazepoint"])
  expect_false(matrix$production_claim_allowed[matrix$vendor == "tobii"])
})

test_that("semantic comparison identifies maximum loss risk", {
  semantics <- data.frame(
    vendor = c("tobii", "gazepoint"), native_field = c("x", "BPOGX"),
    canonical_table = "gaze_samples", canonical_field = "x", transformation = c("identity", "rename"),
    loss_risk = c("none", "low"), stringsAsFactors = FALSE
  )
  comparison <- compare_vendor_semantics(semantics)
  expect_equal(comparison$maximum_loss_risk, "low")
  expect_equal(comparison$vendors, 2L)
})
