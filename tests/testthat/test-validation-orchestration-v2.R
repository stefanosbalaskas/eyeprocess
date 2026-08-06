test_that("validation plans allocate deterministic unique seeds", {
  grid <- list(n = c(20L, 40L), effect = c(0, 0.3))
  x <- validation_job_plan(grid, replications = 3L, base_seed = 42L, model_family = "demo", chunk_size = 2L)
  y <- validation_job_plan(grid, replications = 3L, base_seed = 42L, model_family = "demo", chunk_size = 2L)
  expect_s3_class(x, "eye_validation_job_plan")
  expect_equal(x$jobs$seed, y$jobs$seed)
  expect_equal(nrow(x$jobs), 12L)
  expect_equal(anyDuplicated(x$jobs$job_id), 0L)
  expect_true(all(x$jobs$seed > 0L))
})

test_that("sequential jobs checkpoint, collect, and resume", {
  plan <- validation_job_plan(list(n = c(20L, 30L)), replications = 2L, base_seed = 7L, model_family = "mean")
  directory <- tempfile("eye-validation-")
  simulator <- function(n, seed) { set.seed(seed); structure(rnorm(n, mean = 0.4), truth = c(mu = 0.4)) }
  fitter <- function(x) list(estimate = mean(x), se = sd(x) / sqrt(length(x)), converged = TRUE)
  extractor <- function(fit) data.frame(parameter = "mu", estimate = fit$estimate, std_error = fit$se)
  truth <- function(x) attr(x, "truth")
  run <- run_validation_jobs(plan, simulator, fitter, extractor, truth, directory, progress = FALSE)
  expect_s3_class(run, "eye_validation_run")
  expect_equal(length(run$results), 4L)
  expect_true(all(vapply(run$results, function(x) x$status == "complete", logical(1))))
  collection <- collect_validation_jobs(directory, plan)
  expect_s3_class(collection, "eye_validation_collection")
  expect_equal(nrow(collection$estimates), 4L)
  expect_true(all(collection$estimates$parameter == "mu"))
  resumed <- resume_validation_jobs(plan, directory, simulator = simulator, fitter = fitter, extractor = extractor, truth_extractor = truth, progress = FALSE)
  expect_equal(length(resumed$results), 0L)
})

test_that("failures are retained rather than silently dropped", {
  plan <- validation_job_plan(data.frame(fail = c(FALSE, TRUE)), replications = 1L, base_seed = 10L)
  directory <- tempfile("eye-validation-failure-")
  simulator <- function(fail, seed) list(fail = fail, truth = c(theta = 1))
  fitter <- function(x) { if (x$fail) stop("declared failure"); list(value = 1, converged = TRUE) }
  extractor <- function(fit) c(theta = fit$value)
  truth <- function(x) x$truth
  run_validation_jobs(plan, simulator, fitter, extractor, truth, directory, progress = FALSE)
  collection <- collect_validation_jobs(directory, plan, strict = FALSE)
  expect_equal(sum(collection$jobs$status == "failed"), 1L)
  expect_match(collection$jobs$error[collection$jobs$status == "failed"], "declared failure")
  failure <- validation_failure_summary(collection)
  expect_true(any(failure$failure_rate > 0))
})

test_that("recovery summaries and release reports are reproducible", {
  estimates <- data.frame(parameter = rep("a", 4), estimate = c(.9, 1.0, 1.1, 1.05), truth = 1, std_error = .1, lower = c(.7,.8,.9,.85), upper = c(1.1,1.2,1.3,1.25), status = "complete")
  recovery <- validation_recovery_summary(estimates)
  expect_equal(recovery$replications, 4L)
  expect_true(recovery$rmse > 0)
  expect_true(recovery$coverage >= 0 && recovery$coverage <= 1)
})

test_that("promotion audits default to experimental without evidence", {
  spec <- model_promotion_spec(model_families = c("dynamic_irtree"), require_multi_vendor = TRUE)
  audit <- audit_model_promotion(list(dynamic_irtree = list()), spec)
  expect_s3_class(audit, "eye_model_promotion_audit")
  expect_equal(audit$models$status, "experimental")
  expect_true(audit$models$passed_required_gates < audit$models$required_gates)
})
