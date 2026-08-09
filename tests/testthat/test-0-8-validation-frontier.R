testthat::test_that("validation evidence bundle produces report and manifest", {
  b <- collect_validation_evidence(
    recovery = data.frame(parameter = "a", bias = .01),
    convergence = data.frame(rate = .99),
    model_name = "demo_model"
  )
  testthat::expect_s3_class(b, "eye_validation_bundle")
  m <- validation_bundle_manifest(b)
  testthat::expect_true(any(m$slot == "recovery" & m$status == "available"))
  r <- validation_report(b, include_session = FALSE)
  testthat::expect_true(any(grepl("Convergence is not validation", r, fixed = TRUE)))
})

testthat::test_that("frontier estimators remain gated without external engines", {
  X <- matrix(rbinom(100, 1, .5), 20, 5)
  k <- fit_kde_latent_distribution_irt(X)
  p <- fit_persistence_gaze_diffusion_irt(data.frame(y = 1:5))
  n <- fit_nonignorable_missing_irt(data.frame(y = 1:5))
  testthat::expect_s3_class(k, "eye_gated_process_model")
  testthat::expect_equal(k$status, "gated")
  testthat::expect_equal(p$status, "gated")
  testthat::expect_equal(n$status, "gated")
  testthat::expect_equal(audit_frontier_model_contract(k)$status, "remains_gated")
})

testthat::test_that("structured/unstructured feature contract preserves fold locality rule", {
  d <- data.frame(person_id = paste0("P", 1:12), fold = rep(1:3, each = 4), x = rnorm(12))
  x <- prepare_structured_unstructured_process_features(d, fold = "fold")
  testthat::expect_s3_class(x, "eye_structured_unstructured_process_features")
  testthat::expect_match(x$contract$leakage_rule, "training fold only")
})
