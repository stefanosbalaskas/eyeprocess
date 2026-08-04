test_that("multi-vendor audit distinguishes complete and incomplete evidence", {
  d <- data.frame(
    vendor = c("gazepoint", "gazepoint", "tobii"),
    status = c("pass", "pass", "warning"),
    software_version = c("7.2", "7.2", "1"),
    device_model = c("GP3", "GP3", "Pro"),
    independent_source = c(TRUE, TRUE, TRUE), licence_reviewed = c(TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  spec <- vendor_validation_spec(required_vendors = c("gazepoint", "tobii"), min_cases_per_vendor = 2, min_pass_rate = .9)
  audit <- audit_vendor_validation(d, spec)
  expect_s3_class(audit, "eye_vendor_validation")
  expect_equal(audit$status[audit$vendor == "gazepoint"], "pass")
  expect_equal(audit$status[audit$vendor == "tobii"], "warning")
})

test_that("model validation computes recovery and coverage", {
  simulator <- function(n = 80, beta = .5) {
    x <- rnorm(n); y <- beta * x + rnorm(n)
    list(data = data.frame(x = x, y = y), truth = c(beta = beta))
  }
  fitter <- function(sim) lm(y ~ x, data = sim$data)
  extractor <- function(fit) {
    co <- coef(summary(fit))["x", ]
    data.frame(parameter = "beta", estimate = co[1], std_error = co[2], lower = co[1] - 1.96 * co[2], upper = co[1] + 1.96 * co[2])
  }
  truth <- function(sim) sim$truth
  result <- run_model_validation(simulator, fitter, extractor, truth, grid = data.frame(n = 50, beta = .5), spec = model_validation_spec(replications = 5), seed = 2)
  expect_s3_class(result, "eye_model_validation")
  summary <- model_validation_summary(result)
  expect_equal(summary$replications, 5)
  expect_true(is.finite(summary$rmse))
})

test_that("grouped folds prevent group overlap", {
  d <- expand.grid(participant_id = paste0("P", 1:10), item_id = paste0("I", 1:3))
  d$x <- rnorm(nrow(d)); d$score <- rbinom(nrow(d), 1, plogis(d$x))
  folds <- grouped_folds(d, group = "participant_id", v = 5, seed = 3)
  for (f in folds$folds) {
    expect_length(intersect(d$participant_id[f$analysis], d$participant_id[f$assessment]), 0)
  }
  cv <- grouped_cv(d, score ~ x, group = "participant_id", v = 5, seed = 3)
  expect_s3_class(cv, "eye_grouped_cv")
  expect_equal(nrow(cv$results), 5)
})

test_that("multiverse, benchmark, and reporting audits are executable", {
  mv <- preprocessing_multiverse(
    1:5, list(a = 1, b = 2),
    transform = function(x, spec) x * spec,
    analyse = function(x) data.frame(estimate = mean(x))
  )
  expect_s3_class(mv, "eye_multiverse")
  expect_equal(nrow(mv$results), 2)
  bm <- benchmark_eyeprocess(function() sum(1:100), iterations = 2)
  expect_s3_class(bm, "eye_benchmark")
  x <- simulate_eye_dataset(n_person = 2, n_item = 2, sampling_rate = 10, trial_duration = .3, seed = 104)
  audit <- reporting_guideline_audit(x)
  expect_s3_class(audit, "eye_reporting_audit")
  expect_true(all(c("section", "covered", "status") %in% names(audit)))
})


test_that("coverage remains missing when extractors do not provide intervals", {
  simulator <- function() list(x = rnorm(20), truth = c(mu = 0))
  fitter <- function(sim) mean(sim$x)
  extractor <- function(fit) c(mu = fit)
  result <- run_model_validation(simulator, fitter, extractor, function(sim) sim$truth,
                                 spec = model_validation_spec(replications = 3), seed = 11)
  expect_true(all(is.na(result$runs$covered)))
  expect_true(is.na(model_validation_summary(result)$coverage))
})

test_that("SBC and engine-equivalence harnesses are executable", {
  simulator <- function() list(y = rnorm(30, .25), truth = c(mu = .25))
  fitter <- function(sim) list(mu = mean(sim$y), se = sd(sim$y) / sqrt(length(sim$y)))
  draws <- function(fit) matrix(rnorm(200, fit$mu, fit$se), ncol = 1, dimnames = list(NULL, "mu"))
  sbc <- simulation_based_calibration(simulator, fitter, draws, function(sim) sim$truth, replications = 4, seed = 12)
  expect_s3_class(sbc, "eye_sbc")
  expect_equal(nrow(sbc_summary(sbc)), 1)

  d <- data.frame(x = rnorm(50), y = rnorm(50))
  cmp <- compare_model_engines(
    d,
    engines = list(a = function(z) lm(y ~ x, z), b = function(z) lm(y ~ x, z)),
    extractors = function(fit) c(beta = unname(coef(fit)[["x"]])),
    reference = "a", tolerance = 1e-8
  )
  expect_s3_class(cmp, "eye_engine_comparison")
  expect_true(all(cmp$estimates$equivalent, na.rm = TRUE))
})

test_that("Raven reproduction refuses unreviewed materials", {
  spec <- raven_reproduction_spec(tempfile(), response = "score", strategy_features = c("toggle", "latency"))
  expect_error(
    run_raven_reproduction(spec, identity, function(data, spec) NULL, function(fit) c(beta = 0)),
    "licence_reviewed"
  )
})

test_that("model validation retains heterogeneous failure rows", {
  simulator <- function(mode = "ok") {
    if (mode == "simulation_error") stop("simulation failed")
    list(y = rnorm(10), truth = c(mu = 0), mode = mode)
  }
  fitter <- function(sim) {
    if (identical(sim$mode, "fit_error")) stop("fit failed")
    mean(sim$y)
  }
  result <- run_model_validation(
    simulator, fitter, function(fit) c(mu = fit), function(sim) sim$truth,
    grid = data.frame(mode = c("ok", "simulation_error", "fit_error")),
    spec = model_validation_spec(replications = 1), seed = 13
  )
  expect_equal(nrow(result$runs), 3)
  expect_true(all(c("mode", "scenario", "converged", "error") %in% names(result$runs)))
  expect_true(any(result$runs$parameter == ".simulation"))
  expect_true(any(result$runs$parameter == ".fit"))
  failure_summary <- model_validation_summary(result)
  expect_true(any(failure_summary$status == "fail"))
})


test_that("complete validation programme writes reports and plots", {
  vendors <- c("gazepoint", "tobii", "pupillabs", "eyelink", "smi")
  summary <- data.frame(
    case_id = paste0(rep(vendors, each = 2), "-", rep(1:2, times = length(vendors))),
    vendor = rep(vendors, each = 2), status = "pass",
    software_version = "1.0", device_model = "device",
    independent_source = TRUE, licence_reviewed = TRUE, stringsAsFactors = FALSE
  )
  corpus <- structure(
    list(summary = summary, manifest = summary, status = "pass"),
    class = "eye_corpus_validation"
  )
  x <- simulate_eye_dataset(n_person = 2, n_item = 2, sampling_rate = 10, trial_duration = .3, seed = 14)
  out_dir <- tempfile("validation-program-")
  result <- run_eyeprocess_validation_program(
    corpus, out_dir,
    benchmark_jobs = list(smoke = function() sum(1:100)),
    reporting_dataset = x,
    overwrite = TRUE
  )
  expect_s3_class(result, "eye_validation_program")
  expect_true(file.exists(file.path(out_dir, "vendor-validation.md")))
  expect_true(file.exists(file.path(out_dir, "benchmarks.csv")))
  expect_true(file.exists(file.path(out_dir, "reporting-guideline-audit.csv")))
  expect_true(file.exists(file.path(out_dir, "reporting-guideline-audit.md")))
  expect_true(file.exists(file.path(out_dir, "advanced-model-evidence.csv")))
  expect_true(file.exists(file.path(out_dir, "advanced-model-evidence.md")))
  expect_true(file.exists(file.path(out_dir, "eyeprocess-software-paper.Rmd")))
  expect_true(file.exists(file.path(out_dir, "plots", "vendor-pass-rate.png")))
  expect_true(file.exists(file.path(out_dir, "plots", "benchmarks.png")))
  expect_true(file.exists(file.path(out_dir, "plots", "reporting-guideline-coverage.png")))
  expect_true(file.exists(file.path(out_dir, "plots", "advanced-model-evidence.png")))
})


test_that("public benchmark generator writes a canonical bundle", {
  x <- simulate_eye_dataset(n_person = 3, n_item = 2, sampling_rate = 10, trial_duration = .3, seed = 15)
  out <- tempfile("public-benchmark-")
  path <- create_public_benchmark(x, out, max_participants = 2, include_samples = FALSE, overwrite = TRUE)
  expect_true(dir.exists(path))
  expect_true(file.exists(file.path(path, "manifest.json")))
  expect_true(file.exists(file.path(path, "reporting-guideline-audit.csv")))
})


test_that("advanced model evidence audit prevents premature promotion", {
  spec <- advanced_model_evidence_spec(
    models = c("fit_process_irt", "fit_dynamic_irtree"),
    require_calibration = FALSE,
    require_engine_equivalence = FALSE,
    require_empirical_reproduction = FALSE,
    require_sensitivity = FALSE
  )
  audit <- audit_advanced_model_evidence(list(), spec)
  expect_s3_class(audit, "eye_advanced_evidence_audit")
  expect_true(all(audit$status == "fail"))

  misspecification <- data.frame(expected_failure = TRUE, detected = TRUE)
  recovery <- structure(
    list(
      runs = data.frame(
        scenario = 1, replication = 1, parameter = "beta", estimate = 0.5,
        truth = 0.5, lower = 0.3, upper = 0.7, converged = TRUE,
        error = NA_character_, bias = 0, squared_error = 0, covered = TRUE
      ),
      spec = model_validation_spec(replications = 1), grid = NULL, call = NULL
    ),
    class = "eye_model_validation"
  )
  grouped <- structure(
    list(results = data.frame(fold = 1, n_assessment = 10, score = 0.7), metric = "log_loss"),
    class = "eye_grouped_cv"
  )
  evidence <- list(
    fit_process_irt = list(
      recovery = recovery,
      misspecification = misspecification,
      grouped_validation = grouped
    )
  )
  audit <- audit_advanced_model_evidence(evidence, spec)
  expect_equal(audit$status[audit$model == "fit_process_irt"], "pass")
  expect_equal(audit$status[audit$model == "fit_dynamic_irtree"], "fail")
})


test_that("cross-classified folds exclude held person and item levels", {
  d <- expand.grid(
    participant_id = paste0("P", 1:12),
    item_id = paste0("I", 1:8),
    stringsAsFactors = FALSE
  )
  d$x <- stats::rnorm(nrow(d))
  d$score <- stats::rbinom(nrow(d), 1, stats::plogis(d$x))
  folds <- crossed_grouped_folds(d, groups = c("participant_id", "item_id"), v = 4, seed = 16)
  for (f in folds$folds) {
    expect_length(intersect(d$participant_id[f$analysis], d$participant_id[f$assessment]), 0)
    expect_length(intersect(d$item_id[f$analysis], d$item_id[f$assessment]), 0)
    expect_length(intersect(f$analysis, f$assessment), 0)
  }
  cv <- crossed_grouped_cv(d, score ~ x, groups = c("participant_id", "item_id"), v = 4, seed = 16)
  expect_s3_class(cv, "eye_crossed_grouped_cv")
  expect_equal(nrow(cv$results), 4)
  expect_true(any(is.finite(cv$results$score)))
})

test_that("advanced recovery evidence requires interval coverage", {
  no_intervals <- structure(
    list(
      runs = data.frame(
        scenario = 1, replication = 1, parameter = "beta", estimate = 0.5,
        truth = 0.5, lower = NA_real_, upper = NA_real_, converged = TRUE,
        error = NA_character_, bias = 0, squared_error = 0, covered = NA
      ),
      spec = model_validation_spec(replications = 1), grid = NULL, call = NULL
    ),
    class = "eye_model_validation"
  )
  spec <- advanced_model_evidence_spec(
    models = "fit_process_irt",
    require_calibration = FALSE,
    require_misspecification = FALSE,
    require_grouped_validation = FALSE,
    require_engine_equivalence = FALSE,
    require_empirical_reproduction = FALSE,
    require_sensitivity = FALSE
  )
  audit <- audit_advanced_model_evidence(
    list(fit_process_irt = list(recovery = no_intervals)),
    spec
  )
  expect_equal(audit$status, "fail")
})

test_that("empirical promotion requires published target comparison", {
  reproduction <- structure(
    list(comparison = data.frame(parameter = "beta", estimate = 0.5)),
    class = "eye_empirical_reproduction"
  )
  spec <- advanced_model_evidence_spec(
    models = "fit_process_irt",
    require_recovery = FALSE,
    require_calibration = FALSE,
    require_misspecification = FALSE,
    require_grouped_validation = FALSE,
    require_engine_equivalence = FALSE,
    require_sensitivity = FALSE
  )
  audit <- audit_advanced_model_evidence(
    list(fit_process_irt = list(empirical_reproduction = reproduction)),
    spec
  )
  expect_equal(audit$status, "fail")
})

test_that("leakage audit compares row, single-level, combined, and crossed schemes", {
  d <- expand.grid(
    participant_id = paste0("P", 1:12),
    item_id = paste0("I", 1:8),
    stringsAsFactors = FALSE
  )
  d$x <- stats::rnorm(nrow(d))
  d$score <- stats::rbinom(nrow(d), 1, stats::plogis(d$x))
  audit <- quantify_process_leakage(d, score ~ x, v = 4, seed = 17)
  expect_true(all(c(
    "row_wise", "combined_group", "held_participant_id",
    "held_item_id", "cross_classified"
  ) %in% audit$scheme))
  expect_true(all(c("successful_folds", "mean_log_loss", "optimistic_difference") %in% names(audit)))
})

test_that("model validation records extractor and truth failures", {
  simulator <- function(mode = "extract") list(y = rnorm(10), truth = c(mu = 0), mode = mode)
  fitter <- function(sim) mean(sim$y)
  extractor <- function(fit) stop("extract failed")
  result <- run_model_validation(
    simulator, fitter, extractor, function(sim) sim$truth,
    grid = data.frame(mode = "extract"),
    spec = model_validation_spec(replications = 1), seed = 18
  )
  expect_equal(result$runs$parameter, ".extract")
  expect_false(result$runs$converged)

  result_truth <- run_model_validation(
    simulator, fitter, function(fit) c(mu = fit), function(sim) stop("truth failed"),
    grid = data.frame(mode = "truth"),
    spec = model_validation_spec(replications = 1), seed = 19
  )
  expect_equal(result_truth$runs$parameter, ".truth")
  expect_error(
    run_model_validation(
      simulator, fitter, function(fit) c(mu = fit), function(sim) sim$truth,
      grid = data.frame(), spec = model_validation_spec(replications = 1)
    ),
    "at least one scenario"
  )
})
