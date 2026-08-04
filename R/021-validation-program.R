# Validation-release research programme ---------------------------------------

# Bind heterogeneous result rows without requiring dplyr.
.ep_bind_rows <- function(rows) {
  rows <- Filter(function(x) !is.null(x) && is.data.frame(x), rows)
  if (!length(rows)) return(data.frame())
  columns <- unique(unlist(lapply(rows, names), use.names = FALSE))
  rows <- lapply(rows, function(x) {
    missing <- setdiff(columns, names(x))
    for (nm in missing) x[[nm]] <- NA
    x[columns]
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Specify multi-vendor empirical validation requirements
#'
#' @param required_vendors Vendors that must be represented.
#' @param min_cases_per_vendor Minimum independent cases per vendor.
#' @param min_pass_rate Minimum acceptable pass rate per vendor.
#' @param require_versions Require non-missing software versions.
#' @param require_devices Require non-missing device models.
#' @param require_independent_sources Require each case to be marked as an
#'   independently obtained source rather than a duplicated fixture.
#' @param require_licence_reviewed Require each corpus case to have a completed
#'   data/code licence and redistribution review.
#' @return An `eye_vendor_validation_spec`.
#' @export
vendor_validation_spec <- function(
    required_vendors = c("gazepoint", "tobii", "pupillabs", "eyelink", "smi"),
    min_cases_per_vendor = 2L,
    min_pass_rate = 0.95,
    require_versions = TRUE,
    require_devices = TRUE,
    require_independent_sources = TRUE,
    require_licence_reviewed = TRUE) {
  if (!length(required_vendors) || anyNA(required_vendors) || any(!nzchar(as.character(required_vendors)))) {
    .eye_stop("`required_vendors` must contain non-empty vendor names.")
  }
  if (length(min_cases_per_vendor) != 1L || !is.finite(min_cases_per_vendor) || min_cases_per_vendor < 1) {
    .eye_stop("`min_cases_per_vendor` must be a positive integer.")
  }
  if (length(min_pass_rate) != 1L || !is.finite(min_pass_rate) || min_pass_rate < 0 || min_pass_rate > 1) {
    .eye_stop("`min_pass_rate` must be between zero and one.")
  }
  structure(list(
    required_vendors = tolower(as.character(required_vendors)),
    min_cases_per_vendor = as.integer(min_cases_per_vendor),
    min_pass_rate = as.numeric(min_pass_rate),
    require_versions = isTRUE(require_versions),
    require_devices = isTRUE(require_devices),
    require_independent_sources = isTRUE(require_independent_sources),
    require_licence_reviewed = isTRUE(require_licence_reviewed)
  ), class = "eye_vendor_validation_spec")
}

#' Audit a multi-vendor validation corpus
#'
#' @param x An `eye_corpus_validation` object or its summary data frame.
#' @param spec Validation specification.
#' @return An `eye_vendor_validation` data frame.
#' @export
audit_vendor_validation <- function(x, spec = vendor_validation_spec()) {
  d <- if (inherits(x, "eye_corpus_validation")) x$summary else x
  if (inherits(x, "eye_corpus_validation") && !is.null(x$manifest) && nrow(x$manifest)) {
    match_index <- match(d$case_id, x$manifest$case_id)
    for (field in intersect(c("independent_source", "licence_reviewed"), names(x$manifest))) {
      if (!field %in% names(d)) d[[field]] <- x$manifest[[field]][match_index]
    }
  }
  .assert_data_frame(d, "x")
  required <- c("vendor", "status")
  .assert_columns(d, required)
  d$vendor <- tolower(as.character(d$vendor))
  vendors <- union(spec$required_vendors, unique(d$vendor))
  rows <- lapply(vendors, function(v) {
    z <- d[d$vendor == v, , drop = FALSE]
    n <- nrow(z)
    passes <- sum(z$status == "pass", na.rm = TRUE)
    warnings <- sum(z$status == "warning", na.rm = TRUE)
    failures <- sum(z$status == "fail", na.rm = TRUE)
    pass_rate <- if (n) passes / n else 0
    versions_ok <- !spec$require_versions || ("software_version" %in% names(z) && n > 0 && all(!is.na(z$software_version) & nzchar(as.character(z$software_version))))
    devices_ok <- !spec$require_devices || ("device_model" %in% names(z) && n > 0 && all(!is.na(z$device_model) & nzchar(as.character(z$device_model))))
    independent_flag <- if ("independent_source" %in% names(z)) as.logical(z$independent_source) else rep(NA, n)
    independent_ok <- !spec$require_independent_sources ||
      (n > 0 && all(!is.na(independent_flag) & independent_flag))
    independent_cases <- sum(independent_flag, na.rm = TRUE)
    licence_flag <- if ("licence_reviewed" %in% names(z)) as.logical(z$licence_reviewed) else rep(NA, n)
    licence_ok <- !spec$require_licence_reviewed ||
      (n > 0 && all(!is.na(licence_flag) & licence_flag))
    reviewed_cases <- sum(licence_flag, na.rm = TRUE)
    case_ok <- n >= spec$min_cases_per_vendor
    rate_ok <- pass_rate >= spec$min_pass_rate
    status <- if (case_ok && rate_ok && versions_ok && devices_ok && independent_ok && licence_ok) "pass" else if (n > 0 && failures == 0) "warning" else "fail"
    data.frame(
      vendor = v, cases = n, independent_cases = independent_cases, licence_reviewed_cases = reviewed_cases,
      passes = passes, warnings = warnings, failures = failures,
      pass_rate = pass_rate, cases_sufficient = case_ok, versions_complete = versions_ok,
      devices_complete = devices_ok, independent_sources_complete = independent_ok,
      licences_reviewed = licence_ok, status = status, stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  class(out) <- c("eye_vendor_validation", "data.frame")
  attr(out, "spec") <- spec
  out
}

#' @export
print.eye_vendor_validation <- function(x, ...) {
  cat("Multi-vendor empirical validation audit\n")
  print.data.frame(x, row.names = FALSE)
  invisible(x)
}

#' @export
plot.eye_vendor_validation <- function(x, metric = c("pass_rate", "cases"), ...) {
  metric <- match.arg(metric)
  values <- x[[metric]]
  names(values) <- x$vendor
  graphics::barplot(values, las = 2, ylab = metric, main = "Multi-vendor validation", ...)
  if (metric == "pass_rate") graphics::abline(h = attr(x, "spec")$min_pass_rate, lty = 2)
  invisible(values)
}

#' Write a multi-vendor validation report
#' @param x An `eye_vendor_validation` object.
#' @param path Markdown output file.
#' @return The normalized report path.
#' @export
write_vendor_validation_report <- function(x, path) {
  if (!inherits(x, "eye_vendor_validation")) .eye_stop("Expected an `eye_vendor_validation` object.")
  lines <- c(
    "# Multi-vendor empirical validation audit", "",
    paste0("Generated: ", format(Sys.time(), tz = "UTC", usetz = TRUE)), "",
    "| Vendor | Cases | Independent | Licence reviewed | Passes | Warnings | Failures | Pass rate | Status |",
    "|---|---:|---:|---:|---:|---:|---:|---:|---|",
    vapply(seq_len(nrow(x)), function(i) sprintf(
      "| %s | %d | %d | %d | %d | %d | %d | %.3f | %s |",
      x$vendor[i], x$cases[i], x$independent_cases[i], x$licence_reviewed_cases[i],
      x$passes[i], x$warnings[i], x$failures[i], x$pass_rate[i], x$status[i]
    ), character(1)), "",
    "A fixture-tested adapter is not classified as empirically validated unless independent real-export cases satisfy the declared thresholds."
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path, useBytes = TRUE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

#' Specify a model-validation programme
#'
#' @param replications Number of Monte Carlo replications.
#' @param confidence Confidence level for interval coverage.
#' @param max_abs_bias Maximum acceptable absolute bias.
#' @param min_coverage Minimum acceptable interval coverage.
#' @param max_failure_rate Maximum acceptable estimation failure rate.
#' @return An `eye_model_validation_spec`.
#' @export
model_validation_spec <- function(
    replications = 100L,
    confidence = 0.95,
    max_abs_bias = 0.10,
    min_coverage = 0.90,
    max_failure_rate = 0.05) {
  if (length(replications) != 1L || !is.finite(replications) || replications < 1) {
    .eye_stop("`replications` must be a positive integer.")
  }
  bounded <- c(confidence = confidence, min_coverage = min_coverage, max_failure_rate = max_failure_rate)
  if (any(!is.finite(bounded)) || any(bounded < 0) || any(bounded > 1)) {
    .eye_stop("Confidence, coverage, and failure-rate thresholds must be between zero and one.")
  }
  if (length(max_abs_bias) != 1L || !is.finite(max_abs_bias) || max_abs_bias < 0) {
    .eye_stop("`max_abs_bias` must be a finite non-negative value.")
  }
  structure(list(
    replications = as.integer(replications), confidence = as.numeric(confidence),
    max_abs_bias = as.numeric(max_abs_bias), min_coverage = as.numeric(min_coverage),
    max_failure_rate = as.numeric(max_failure_rate)
  ), class = "eye_model_validation_spec")
}

.ep_expand_grid_rows <- function(grid) {
  if (is.null(grid)) return(list(list()))
  if (is.data.frame(grid)) {
    if (!nrow(grid)) .eye_stop("`grid` must contain at least one scenario.")
    return(lapply(seq_len(nrow(grid)), function(i) as.list(grid[i, , drop = FALSE])))
  }
  if (is.list(grid)) {
    expanded <- do.call(expand.grid, c(grid, stringsAsFactors = FALSE))
    if (!nrow(expanded)) .eye_stop("`grid` must contain at least one scenario.")
    return(lapply(seq_len(nrow(expanded)), function(i) as.list(expanded[i, , drop = FALSE])))
  }
  .eye_stop("`grid` must be a data frame, list, or NULL.")
}

#' Run parameter-recovery, coverage, and misspecification validation
#'
#' The fitter may return a model or throw an error. The extractor must return a
#' data frame with `parameter`, `estimate`, and optionally `std_error`, `lower`,
#' and `upper`. The truth extractor must return a named numeric vector.
#'
#' @param simulator Simulation function.
#' @param fitter Estimation function receiving the simulation result.
#' @param extractor Parameter extraction function.
#' @param truth_extractor Truth extraction function.
#' @param grid Scenario grid.
#' @param spec Validation specification.
#' @param seed Random seed.
#' @param continue_on_error Record rather than stop on estimation errors.
#' @return An `eye_model_validation` object.
#' @export
run_model_validation <- function(
    simulator,
    fitter,
    extractor,
    truth_extractor,
    grid = NULL,
    spec = model_validation_spec(),
    seed = 1L,
    continue_on_error = TRUE) {
  funs <- list(simulator = simulator, fitter = fitter, extractor = extractor, truth_extractor = truth_extractor)
  if (!all(vapply(funs, is.function, logical(1)))) .eye_stop("All simulator/fitter/extractor arguments must be functions.")
  scenarios <- .ep_expand_grid_rows(grid)
  set.seed(seed)
  rows <- list(); run <- 0L
  for (s in seq_along(scenarios)) {
    scenario <- scenarios[[s]]
    for (r in seq_len(spec$replications)) {
      run <- run + 1L
      sim_seed <- sample.int(.Machine$integer.max, 1L)
      set.seed(sim_seed)
      sim <- tryCatch(do.call(simulator, scenario), error = identity)
      if (inherits(sim, "error")) {
        failure <- data.frame(
          scenario = s, replication = r, parameter = ".simulation",
          estimate = NA_real_, truth = NA_real_, lower = NA_real_, upper = NA_real_,
          converged = FALSE, error = conditionMessage(sim), stringsAsFactors = FALSE
        )
        for (nm in names(scenario)) failure[[nm]] <- scenario[[nm]]
        rows[[run]] <- failure
        if (!continue_on_error) stop(sim)
        next
      }
      fit <- tryCatch(fitter(sim), error = identity)
      if (inherits(fit, "error")) {
        failure <- data.frame(
          scenario = s, replication = r, parameter = ".fit",
          estimate = NA_real_, truth = NA_real_, lower = NA_real_, upper = NA_real_,
          converged = FALSE, error = conditionMessage(fit), stringsAsFactors = FALSE
        )
        for (nm in names(scenario)) failure[[nm]] <- scenario[[nm]]
        rows[[run]] <- failure
        if (!continue_on_error) stop(fit)
        next
      }
      est <- tryCatch(extractor(fit), error = identity)
      if (inherits(est, "error")) {
        failure <- data.frame(
          scenario = s, replication = r, parameter = ".extract",
          estimate = NA_real_, truth = NA_real_, lower = NA_real_, upper = NA_real_,
          converged = FALSE, error = conditionMessage(est), stringsAsFactors = FALSE
        )
        for (nm in names(scenario)) failure[[nm]] <- scenario[[nm]]
        rows[[run]] <- failure
        if (!continue_on_error) stop(est)
        next
      }
      if (is.numeric(est) && !is.null(names(est))) est <- data.frame(parameter = names(est), estimate = as.numeric(est), stringsAsFactors = FALSE)
      if (!is.data.frame(est) || !all(c("parameter", "estimate") %in% names(est))) {
        failure <- data.frame(
          scenario = s, replication = r, parameter = ".extract",
          estimate = NA_real_, truth = NA_real_, lower = NA_real_, upper = NA_real_,
          converged = FALSE, error = "Extractor must return a data frame with parameter and estimate columns.",
          stringsAsFactors = FALSE
        )
        for (nm in names(scenario)) failure[[nm]] <- scenario[[nm]]
        rows[[run]] <- failure
        if (!continue_on_error) .eye_stop(failure$error[[1L]])
        next
      }
      truth_raw <- tryCatch(truth_extractor(sim), error = identity)
      if (inherits(truth_raw, "error")) {
        failure <- data.frame(
          scenario = s, replication = r, parameter = ".truth",
          estimate = NA_real_, truth = NA_real_, lower = NA_real_, upper = NA_real_,
          converged = FALSE, error = conditionMessage(truth_raw), stringsAsFactors = FALSE
        )
        for (nm in names(scenario)) failure[[nm]] <- scenario[[nm]]
        rows[[run]] <- failure
        if (!continue_on_error) stop(truth_raw)
        next
      }
      if (is.list(truth_raw)) truth_raw <- unlist(truth_raw)
      if (is.null(names(truth_raw))) {
        failure <- data.frame(
          scenario = s, replication = r, parameter = ".truth",
          estimate = NA_real_, truth = NA_real_, lower = NA_real_, upper = NA_real_,
          converged = FALSE, error = "Truth extractor must return named values.", stringsAsFactors = FALSE
        )
        for (nm in names(scenario)) failure[[nm]] <- scenario[[nm]]
        rows[[run]] <- failure
        if (!continue_on_error) .eye_stop(failure$error[[1L]])
        next
      }
      truth <- as.numeric(truth_raw); names(truth) <- names(truth_raw)
      est$truth <- unname(truth[match(est$parameter, names(truth))])
      if (!"lower" %in% names(est)) est$lower <- NA_real_
      if (!"upper" %in% names(est)) est$upper <- NA_real_
      est$scenario <- s; est$replication <- r; est$converged <- TRUE; est$error <- NA_character_
      for (nm in names(scenario)) est[[nm]] <- scenario[[nm]]
      rows[[run]] <- est
    }
  }
  runs <- .ep_bind_rows(rows)
  runs$bias <- runs$estimate - runs$truth
  runs$squared_error <- runs$bias^2
  interval_available <- is.finite(runs$lower) & is.finite(runs$upper) & is.finite(runs$truth)
  runs$covered <- ifelse(interval_available, runs$lower <= runs$truth & runs$upper >= runs$truth, NA)
  out <- list(runs = runs, spec = spec, grid = grid, call = match.call())
  class(out) <- "eye_model_validation"
  out
}

#' Summarize model validation
#' @param x An `eye_model_validation` object.
#' @return Scenario-by-parameter validation metrics.
#' @export
model_validation_summary <- function(x) {
  if (!inherits(x, "eye_model_validation")) .eye_stop("Expected an `eye_model_validation` object.")
  d <- x$runs
  scenario_columns <- setdiff(names(d), c("replication", "parameter", "estimate", "truth", "std_error", "lower", "upper", "converged", "error", "bias", "squared_error", "covered"))
  keys <- intersect(c("scenario", scenario_columns, "parameter"), names(d))
  split_key <- interaction(d[keys], drop = TRUE, lex.order = TRUE)
  groups <- split(d, split_key)
  out <- lapply(groups, function(z) {
    ok <- z$converged & is.finite(z$estimate) & is.finite(z$truth)
    base <- z[1L, keys, drop = FALSE]
    cbind(base, data.frame(
      replications = length(unique(z$replication)), successful = sum(ok),
      failure_rate = mean(!z$converged), bias = if (any(ok)) mean(z$bias[ok]) else NA_real_,
      absolute_bias = if (any(ok)) mean(abs(z$bias[ok])) else NA_real_,
      rmse = if (any(ok)) sqrt(mean(z$squared_error[ok])) else NA_real_,
      coverage = if (any(ok & !is.na(z$covered))) mean(z$covered[ok & !is.na(z$covered)]) else NA_real_,
      stringsAsFactors = FALSE
    ))
  })
  out <- do.call(rbind, out); rownames(out) <- NULL
  out$status <- ifelse(
    out$successful < 1L |
      !is.finite(out$absolute_bias) |
      out$failure_rate > x$spec$max_failure_rate |
      out$absolute_bias > x$spec$max_abs_bias |
      (is.finite(out$coverage) & out$coverage < x$spec$min_coverage),
    "fail", "pass"
  )
  out
}

#' @export
print.eye_model_validation <- function(x, ...) {
  cat("Advanced-model validation programme\n")
  print(model_validation_summary(x), row.names = FALSE)
  invisible(x)
}

#' @export
plot.eye_model_validation <- function(x, type = c("recovery", "bias", "coverage"), ...) {
  type <- match.arg(type)
  d <- x$runs[x$runs$converged & is.finite(x$runs$estimate) & is.finite(x$runs$truth), , drop = FALSE]
  if (!nrow(d)) .eye_stop("No successful validation runs are available.")
  if (type == "recovery") {
    graphics::plot(d$truth, d$estimate, xlab = "Truth", ylab = "Estimate", main = "Parameter recovery", ...)
    graphics::abline(0, 1, lty = 2)
  } else {
    s <- model_validation_summary(x)
    value <- if (type == "bias") s$bias else s$coverage
    names(value) <- paste(s$scenario, s$parameter, sep = ":")
    graphics::barplot(value, las = 2, main = paste("Model", type), ylab = type, ...)
    if (type == "coverage") graphics::abline(h = x$spec$min_coverage, lty = 2)
  }
  invisible(d)
}


#' Specify evidence required to promote advanced model interfaces
#'
#' @param models Advanced model function names.
#' @param require_recovery Require passing parameter-recovery evidence.
#' @param require_calibration Require simulation-based calibration evidence.
#' @param require_misspecification Require evidence that prespecified failure
#'   scenarios are detected.
#' @param require_grouped_validation Require grouped out-of-sample validation.
#' @param require_engine_equivalence Require comparison with a benchmark engine.
#' @param require_empirical_reproduction Require a licensed empirical reproduction.
#' @param require_sensitivity Require a multi-specification preprocessing/AOI sensitivity analysis.
#' @return An `eye_advanced_evidence_spec`.
#' @export
advanced_model_evidence_spec <- function(
    models = c(
      "fit_joint_process_model", "fit_shared_process_factor", "fit_strategy_mixture",
      "fit_process_irt", "fit_pupil_informed_irt", "fit_multimodal_irt",
      "fit_dynamic_aoi_model", "fit_gaze_weighted_choice",
      "fit_dynamic_irtree", "fit_joint_functional_pupil_irt",
      "fit_theory_strategy_irt", "fit_gaze_diffusion_irt"
    ),
    require_recovery = TRUE,
    require_calibration = TRUE,
    require_misspecification = TRUE,
    require_grouped_validation = TRUE,
    require_engine_equivalence = TRUE,
    require_empirical_reproduction = TRUE,
    require_sensitivity = TRUE) {
  if (!length(models) || anyNA(models) || any(!nzchar(as.character(models)))) {
    .eye_stop("`models` must contain non-empty function names.")
  }
  structure(list(
    models = unique(as.character(models)),
    require_recovery = isTRUE(require_recovery),
    require_calibration = isTRUE(require_calibration),
    require_misspecification = isTRUE(require_misspecification),
    require_grouped_validation = isTRUE(require_grouped_validation),
    require_engine_equivalence = isTRUE(require_engine_equivalence),
    require_empirical_reproduction = isTRUE(require_empirical_reproduction),
    require_sensitivity = isTRUE(require_sensitivity)
  ), class = "eye_advanced_evidence_spec")
}

.ep_recovery_pass <- function(x) {
  if (!inherits(x, "eye_model_validation")) return(FALSE)
  z <- model_validation_summary(x)
  nrow(z) > 0L &&
    all(z$status == "pass") &&
    all(is.finite(z$coverage)) &&
    all(z$coverage >= x$spec$min_coverage)
}

.ep_sbc_pass <- function(x) {
  if (!inherits(x, "eye_sbc")) return(FALSE)
  z <- sbc_summary(x)
  nrow(z) > 0L && all(z$status == "pass")
}

.ep_misspecification_pass <- function(x) {
  if (inherits(x, "eye_model_validation")) {
    z <- model_validation_summary(x)
    if (!"expected_failure" %in% names(z)) return(FALSE)
    expected <- as.logical(z$expected_failure)
    return(any(expected, na.rm = TRUE) && all(z$status[expected] == "fail", na.rm = TRUE))
  }
  if (is.data.frame(x) && all(c("expected_failure", "detected") %in% names(x))) {
    expected <- as.logical(x$expected_failure)
    detected <- as.logical(x$detected)
    return(any(expected, na.rm = TRUE) && all(detected[expected], na.rm = TRUE))
  }
  FALSE
}

.ep_grouped_validation_pass <- function(x) {
  if (!inherits(x, "eye_grouped_cv") && !inherits(x, "eye_crossed_grouped_cv")) return(FALSE)
  errors <- if ("error" %in% names(x$results)) x$results$error else rep(NA_character_, nrow(x$results))
  nrow(x$results) > 0L &&
    "score" %in% names(x$results) &&
    all(is.finite(x$results$score)) &&
    all(is.na(errors) | !nzchar(errors))
}

.ep_engine_pass <- function(x) {
  inherits(x, "eye_engine_comparison") && nrow(x$estimates) > 0L &&
    all(x$estimates$equivalent[!is.na(x$estimates$equivalent)]) &&
    any(!is.na(x$estimates$equivalent))
}

.ep_empirical_pass <- function(x) {
  if (!inherits(x, "eye_empirical_reproduction")) return(FALSE)
  z <- x$comparison
  if (!all(c("target", "absolute_difference", "reproduced") %in% names(z))) return(FALSE)
  any(!is.na(z$reproduced)) && all(z$reproduced[!is.na(z$reproduced)])
}

.ep_sensitivity_pass <- function(x) {
  if (!inherits(x, "eye_multiverse") || length(x$specifications) < 2L || !nrow(x$results)) return(FALSE)
  errors <- if ("error" %in% names(x$results)) x$results$error else rep(NA_character_, nrow(x$results))
  numeric_columns <- names(x$results)[vapply(x$results, is.numeric, logical(1))]
  any(vapply(x$results[numeric_columns], function(z) any(is.finite(z)), logical(1))) &&
    all(is.na(errors) | !nzchar(errors))
}

#' Audit advanced-model scientific evidence
#'
#' @param evidence Named list keyed by model function. Each model may contain
#'   `recovery`, `calibration`, `misspecification`, `grouped_validation`,
#'   `engine_equivalence`, `empirical_reproduction`, and `sensitivity` objects.
#' @param spec Evidence specification.
#' @return An `eye_advanced_evidence_audit` data frame.
#' @export
audit_advanced_model_evidence <- function(
    evidence,
    spec = advanced_model_evidence_spec()) {
  if (!is.list(evidence)) .eye_stop("`evidence` must be a named list.")
  rows <- lapply(spec$models, function(model) {
    record <- evidence[[model]] %||% list()
    if (!is.list(record)) record <- list()
    recovery <- .ep_recovery_pass(record$recovery)
    calibration <- .ep_sbc_pass(record$calibration)
    misspecification <- .ep_misspecification_pass(record$misspecification)
    grouped <- .ep_grouped_validation_pass(record$grouped_validation)
    engine <- .ep_engine_pass(record$engine_equivalence)
    empirical <- .ep_empirical_pass(record$empirical_reproduction)
    sensitivity <- .ep_sensitivity_pass(record$sensitivity)
    observed <- c(
      recovery = recovery,
      calibration = calibration,
      misspecification = misspecification,
      grouped_validation = grouped,
      engine_equivalence = engine,
      empirical_reproduction = empirical,
      sensitivity = sensitivity
    )
    required_flags <- c(
      recovery = spec$require_recovery,
      calibration = spec$require_calibration,
      misspecification = spec$require_misspecification,
      grouped_validation = spec$require_grouped_validation,
      engine_equivalence = spec$require_engine_equivalence,
      empirical_reproduction = spec$require_empirical_reproduction,
      sensitivity = spec$require_sensitivity
    )
    required_results <- observed[required_flags]
    completed <- sum(required_results)
    required_n <- length(required_results)
    status <- if (!required_n || all(required_results)) {
      "pass"
    } else if (any(required_results)) {
      "warning"
    } else {
      "fail"
    }
    data.frame(
      model = model, recovery = recovery, calibration = calibration,
      misspecification = misspecification, grouped_validation = grouped,
      engine_equivalence = engine, empirical_reproduction = empirical,
      sensitivity = sensitivity,
      completed = completed, required = required_n, status = status,
      stringsAsFactors = FALSE
    )
  })
  out <- .ep_bind_rows(rows)
  class(out) <- c("eye_advanced_evidence_audit", "data.frame")
  attr(out, "spec") <- spec
  out
}

#' @export
print.eye_advanced_evidence_audit <- function(x, ...) {
  cat("Advanced-model scientific-evidence audit\n")
  print.data.frame(x, row.names = FALSE)
  invisible(x)
}

#' @export
plot.eye_advanced_evidence_audit <- function(x, ...) {
  proportion <- ifelse(x$required > 0L, x$completed / x$required, 1)
  names(proportion) <- x$model
  graphics::barplot(proportion, las = 2, ylim = c(0, 1),
                    ylab = "Required evidence completed", main = "Advanced-model evidence", ...)
  graphics::abline(h = 1, lty = 2)
  invisible(proportion)
}

#' Write an advanced-model evidence report
#' @param x An `eye_advanced_evidence_audit` object.
#' @param path Markdown output file.
#' @return Normalized report path.
#' @export
write_advanced_model_evidence_report <- function(x, path) {
  if (!inherits(x, "eye_advanced_evidence_audit")) .eye_stop("Expected an advanced-model evidence audit.")
  lines <- c(
    "# Advanced-model scientific-evidence audit", "",
    paste0("Generated: ", format(Sys.time(), tz = "UTC", usetz = TRUE)), "",
    "| Model | Recovery | Calibration | Misspecification | Grouped validation | Engine equivalence | Empirical reproduction | Sensitivity | Status |",
    "|---|---|---|---|---|---|---|---|---|",
    vapply(seq_len(nrow(x)), function(i) sprintf(
      "| `%s()` | %s | %s | %s | %s | %s | %s | %s | %s |",
      x$model[i], x$recovery[i], x$calibration[i], x$misspecification[i],
      x$grouped_validation[i], x$engine_equivalence[i], x$empirical_reproduction[i],
      x$sensitivity[i], x$status[i]
    ), character(1)), "",
    "A model remains experimental until every required evidence gate is satisfied."
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path, useBytes = TRUE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

#' Run simulation-based calibration
#'
#' @param simulator Function returning simulated data and named truth.
#' @param fitter Function receiving one simulation result.
#' @param posterior_draws Function returning a numeric matrix/data frame whose
#'   columns are named parameters.
#' @param truth_extractor Function returning a named numeric truth vector.
#' @param replications Number of simulated data sets.
#' @param seed Random seed.
#' @param ... Passed to `simulator()`.
#' @return An `eye_sbc` object with parameter ranks and calibration summaries.
#' @export
simulation_based_calibration <- function(
    simulator, fitter, posterior_draws, truth_extractor,
    replications = 100L, seed = 1L, ...) {
  funs <- list(simulator, fitter, posterior_draws, truth_extractor)
  if (!all(vapply(funs, is.function, logical(1)))) .eye_stop("All SBC components must be functions.")
  replications <- as.integer(replications)
  if (length(replications) != 1L || is.na(replications) || replications < 1L) {
    .eye_stop("`replications` must be a positive integer.")
  }
  set.seed(seed)
  rows <- vector("list", replications)
  for (r in seq_len(replications)) {
    sim <- tryCatch(simulator(...), error = identity)
    if (inherits(sim, "error")) {
      rows[[r]] <- data.frame(replication = r, parameter = NA_character_, rank = NA_integer_, draws = NA_integer_, normalized_rank = NA_real_, truth = NA_real_, posterior_mean = NA_real_, posterior_sd = NA_real_, error = conditionMessage(sim))
      next
    }
    fit <- tryCatch(fitter(sim), error = identity)
    if (inherits(fit, "error")) {
      rows[[r]] <- data.frame(replication = r, parameter = NA_character_, rank = NA_integer_, draws = NA_integer_, normalized_rank = NA_real_, truth = NA_real_, posterior_mean = NA_real_, posterior_sd = NA_real_, error = conditionMessage(fit))
      next
    }
    draws <- tryCatch(as.matrix(posterior_draws(fit)), error = identity)
    truth <- truth_extractor(sim)
    if (inherits(draws, "error") || is.null(colnames(draws)) || is.null(names(truth))) {
      message <- if (inherits(draws, "error")) conditionMessage(draws) else "Posterior draws and truth must have matching parameter names."
      rows[[r]] <- data.frame(replication = r, parameter = NA_character_, rank = NA_integer_, draws = NA_integer_, normalized_rank = NA_real_, truth = NA_real_, posterior_mean = NA_real_, posterior_sd = NA_real_, error = message)
      next
    }
    parameters <- intersect(colnames(draws), names(truth))
    if (!length(parameters)) {
      rows[[r]] <- data.frame(replication = r, parameter = NA_character_, rank = NA_integer_, draws = nrow(draws), normalized_rank = NA_real_, truth = NA_real_, posterior_mean = NA_real_, posterior_sd = NA_real_, error = "No matching posterior/truth parameters.")
      next
    }
    rows[[r]] <- do.call(rbind, lapply(parameters, function(p) {
      z <- draws[, p]
      z <- z[is.finite(z)]
      rank <- if (length(z) && is.finite(truth[[p]])) {
        below <- sum(z < truth[[p]])
        tied <- sum(z == truth[[p]])
        below + if (tied) sample.int(tied + 1L, 1L) - 1L else 0L
      } else {
        NA_integer_
      }
      data.frame(
        replication = r, parameter = p, rank = rank, draws = length(z),
        normalized_rank = if (length(z)) (rank + 0.5) / (length(z) + 1) else NA_real_,
        truth = unname(truth[[p]]), posterior_mean = if (length(z)) mean(z) else NA_real_,
        posterior_sd = if (length(z) > 1L) stats::sd(z) else NA_real_, error = NA_character_,
        stringsAsFactors = FALSE
      )
    }))
  }
  out <- list(ranks = .ep_bind_rows(rows), replications = as.integer(replications), seed = seed, call = match.call())
  class(out) <- "eye_sbc"
  out
}

#' Summarize simulation-based calibration
#' @param x An `eye_sbc` object.
#' @return Parameter-level rank and standardized-bias summaries.
#' @export
sbc_summary <- function(x) {
  if (!inherits(x, "eye_sbc")) .eye_stop("Expected an `eye_sbc` object.")
  d <- x$ranks
  groups <- split(d, d$parameter)
  out <- lapply(groups[names(groups) != "NA"], function(z) {
    usable <- is.finite(z$normalized_rank)
    standardized <- (z$posterior_mean - z$truth) / z$posterior_sd
    uniformity_p <- if (sum(usable) >= 20L) {
      suppressWarnings(stats::ks.test(z$normalized_rank[usable], "punif")$p.value)
    } else {
      NA_real_
    }
    mean_rank <- if (any(usable)) mean(z$normalized_rank[usable]) else NA_real_
    status <- if (sum(usable) < 20L) {
      "insufficient"
    } else if (is.finite(mean_rank) && abs(mean_rank - 0.5) <= 0.10 &&
               is.finite(uniformity_p) && uniformity_p >= 0.01) {
      "pass"
    } else {
      "fail"
    }
    data.frame(
      parameter = z$parameter[[1L]], replications = nrow(z), successful = sum(usable),
      mean_rank = mean_rank,
      rank_variance = if (sum(usable) > 1L) stats::var(z$normalized_rank[usable]) else NA_real_,
      uniformity_p_value = uniformity_p,
      mean_standardized_bias = if (any(is.finite(standardized))) mean(standardized[is.finite(standardized)]) else NA_real_,
      status = status,
      stringsAsFactors = FALSE
    )
  })
  if (!length(out)) return(data.frame())
  do.call(rbind, out)
}

#' @export
print.eye_sbc <- function(x, ...) {
  cat("Simulation-based calibration\n")
  print(sbc_summary(x), row.names = FALSE)
  invisible(x)
}

#' @export
plot.eye_sbc <- function(x, parameter = NULL, breaks = 10L, ...) {
  d <- x$ranks
  if (is.null(parameter)) parameter <- unique(stats::na.omit(d$parameter))[[1L]]
  z <- d$normalized_rank[d$parameter == parameter]
  z <- z[is.finite(z)]
  if (!length(z)) .eye_stop("No usable SBC ranks for parameter: ", parameter)
  graphics::hist(z, breaks = breaks, xlim = c(0, 1), xlab = "Normalized rank", main = paste("SBC:", parameter), ...)
  invisible(z)
}

#' Compare equivalent model engines
#'
#' @param data Model-ready data.
#' @param engines Named list of fitting functions receiving `data`.
#' @param extractors Named list of extractor functions or one shared extractor.
#' @param reference Optional reference engine name.
#' @param tolerance Maximum absolute estimate difference for equivalence.
#' @return An `eye_engine_comparison` object.
#' @export
compare_model_engines <- function(data, engines, extractors, reference = names(engines)[1L], tolerance = 0.05) {
  if (!is.list(engines) || !length(engines) || is.null(names(engines)) || !all(vapply(engines, is.function, logical(1)))) {
    .eye_stop("`engines` must be a named list of fitting functions.")
  }
  if (is.function(extractors)) extractors <- setNames(rep(list(extractors), length(engines)), names(engines))
  if (!is.list(extractors) || !all(names(engines) %in% names(extractors)) ||
      !all(vapply(extractors[names(engines)], is.function, logical(1)))) {
    .eye_stop("Supply one extractor function per engine or one shared extractor.")
  }
  fits <- lapply(engines, function(fun) tryCatch(fun(data), error = identity))
  estimates <- lapply(names(fits), function(nm) {
    fit <- fits[[nm]]
    if (inherits(fit, "error")) return(data.frame(engine = nm, parameter = NA_character_, estimate = NA_real_, error = conditionMessage(fit)))
    z <- extractors[[nm]](fit)
    if (is.numeric(z) && !is.null(names(z))) z <- data.frame(parameter = names(z), estimate = as.numeric(z))
    .assert_data_frame(z, paste0("extractor result for ", nm))
    .assert_columns(z, c("parameter", "estimate"))
    z$engine <- nm; z$error <- NA_character_; z
  })
  estimates <- .ep_bind_rows(estimates)
  ref <- estimates[estimates$engine == reference & !is.na(estimates$parameter), c("parameter", "estimate")]
  names(ref)[2L] <- "reference_estimate"
  estimates <- merge(estimates, ref, by = "parameter", all.x = TRUE)
  estimates$absolute_difference <- abs(estimates$estimate - estimates$reference_estimate)
  estimates$equivalent <- ifelse(is.finite(estimates$absolute_difference), estimates$absolute_difference <= tolerance, NA)
  out <- list(fits = fits, estimates = estimates, reference = reference, tolerance = tolerance)
  class(out) <- "eye_engine_comparison"
  out
}

#' @export
print.eye_engine_comparison <- function(x, ...) {
  cat("Equivalent-engine comparison\n")
  cat("Reference: ", x$reference, "; tolerance: ", x$tolerance, "\n", sep = "")
  print(x$estimates, row.names = FALSE)
  invisible(x)
}

#' @export
plot.eye_engine_comparison <- function(x, parameter = NULL, ...) {
  d <- x$estimates
  if (is.null(parameter)) parameter <- unique(stats::na.omit(d$parameter))[[1L]]
  z <- d[d$parameter == parameter, , drop = FALSE]
  values <- z$estimate; names(values) <- z$engine
  graphics::dotchart(values, xlab = "Estimate", main = paste("Engine comparison:", parameter), ...)
  reference_value <- unique(stats::na.omit(z$reference_estimate))
  if (length(reference_value)) graphics::abline(v = reference_value[[1L]], lty = 2)
  invisible(z)
}

#' Specify a published Raven strategy-model reproduction
#'
#' @param data_path Path to the exact public data/materials.
#' @param response Response field.
#' @param strategy_features Theory-defined eye-tracking strategy indicators.
#' @param published_targets Optional named target estimates.
#' @param licence_reviewed Whether data and code reuse has been reviewed.
#' @param citation Citation or DOI for the reproduced analysis.
#' @return An `eye_raven_reproduction_spec`.
#' @export
raven_reproduction_spec <- function(
    data_path, response, strategy_features, published_targets = NULL,
    licence_reviewed = FALSE, citation = "10.1016/j.intell.2023.101782") {
  if (length(data_path) != 1L || is.na(data_path) || !nzchar(data_path)) .eye_stop("`data_path` must be a non-empty path.")
  if (length(response) != 1L || is.na(response) || !nzchar(response)) .eye_stop("`response` must be a non-empty column name.")
  if (!length(strategy_features) || anyNA(strategy_features) || any(!nzchar(strategy_features))) {
    .eye_stop("`strategy_features` must contain non-empty column names.")
  }
  if (!is.null(published_targets)) {
    if (is.list(published_targets)) published_targets <- unlist(published_targets)
    if (!is.numeric(published_targets) || is.null(names(published_targets)) || any(!is.finite(published_targets))) {
      .eye_stop("`published_targets` must be a named finite numeric vector.")
    }
  }
  structure(list(
    data_path = normalizePath(data_path, winslash = "/", mustWork = FALSE),
    response = response, strategy_features = as.character(strategy_features),
    published_targets = published_targets, licence_reviewed = isTRUE(licence_reviewed), citation = citation
  ), class = "eye_raven_reproduction_spec")
}

#' Execute a licensed published-model reproduction
#'
#' @param spec Reproduction specification.
#' @param importer Function receiving `spec$data_path`.
#' @param fitter Function receiving imported data and `spec`.
#' @param extractor Function returning named estimates or a parameter/estimate data frame.
#' @param tolerance Absolute target tolerance.
#' @return An `eye_empirical_reproduction` object.
#' @export
run_raven_reproduction <- function(spec, importer, fitter, extractor, tolerance = 0.05) {
  if (!inherits(spec, "eye_raven_reproduction_spec")) .eye_stop("`spec` must be created by `raven_reproduction_spec()`.")
  if (length(tolerance) != 1L || !is.finite(tolerance) || tolerance < 0) .eye_stop("`tolerance` must be a finite non-negative value.")
  if (!isTRUE(spec$licence_reviewed)) .eye_stop("Set `licence_reviewed = TRUE` only after reviewing the exact public data/code terms.")
  if (!file.exists(spec$data_path) && !dir.exists(spec$data_path)) .eye_stop("Raven reproduction materials were not found: ", spec$data_path)
  if (!all(vapply(list(importer, fitter, extractor), is.function, logical(1)))) .eye_stop("Importer, fitter, and extractor must be functions.")
  data <- importer(spec$data_path)
  fit <- fitter(data, spec)
  estimates <- extractor(fit)
  if (is.numeric(estimates) && !is.null(names(estimates))) estimates <- data.frame(parameter = names(estimates), estimate = as.numeric(estimates))
  .assert_data_frame(estimates, "reproduction estimates")
  .assert_columns(estimates, c("parameter", "estimate"))
  targets <- spec$published_targets
  comparison <- estimates
  if (!is.null(targets)) {
    if (is.list(targets)) targets <- unlist(targets)
    comparison$target <- unname(targets[match(comparison$parameter, names(targets))])
    comparison$absolute_difference <- abs(comparison$estimate - comparison$target)
    comparison$reproduced <- ifelse(is.finite(comparison$absolute_difference), comparison$absolute_difference <= tolerance, NA)
  }
  out <- list(spec = spec, data = data, fit = fit, comparison = comparison, tolerance = tolerance)
  class(out) <- "eye_empirical_reproduction"
  out
}

#' @export
print.eye_empirical_reproduction <- function(x, ...) {
  cat("Published Raven strategy-model reproduction\n")
  cat("Citation: ", x$spec$citation, "\n", sep = "")
  print(x$comparison, row.names = FALSE)
  invisible(x)
}

#' @export
plot.eye_empirical_reproduction <- function(x, ...) {
  d <- x$comparison
  if (!all(c("target", "estimate") %in% names(d))) {
    .eye_stop("Published targets are required for the reproduction plot.")
  }
  usable <- is.finite(d$target) & is.finite(d$estimate)
  if (!any(usable)) .eye_stop("No finite target/estimate pairs are available.")
  graphics::plot(
    d$target[usable], d$estimate[usable],
    xlab = "Published target", ylab = "Reproduced estimate",
    main = "Empirical reproduction", ...
  )
  graphics::abline(0, 1, lty = 2)
  invisible(d[usable, , drop = FALSE])
}

#' Create grouped cross-validation folds
#' @param data Data frame.
#' @param group Grouping columns that must not cross folds.
#' @param v Number of folds.
#' @param seed Random seed.
#' @return An `eye_grouped_folds` object.
#' @export
grouped_folds <- function(data, group = c("participant_id"), v = 5L, seed = 1L) {
  .assert_data_frame(data, "data")
  if (!length(group) || anyNA(group) || any(!nzchar(group))) .eye_stop("`group` must contain non-empty column names.")
  .assert_columns(data, group)
  v <- as.integer(v)
  if (length(v) != 1L || is.na(v) || v < 2L) .eye_stop("`v` must be an integer of at least two.")
  key <- interaction(data[group], drop = TRUE, lex.order = TRUE)
  levels_key <- unique(as.character(key))
  if (length(levels_key) < v) .eye_stop("There are fewer independent groups than requested folds.")
  set.seed(seed)
  assignment <- sample(rep(seq_len(v), length.out = length(levels_key)))
  names(assignment) <- levels_key
  fold_id <- unname(assignment[as.character(key)])
  out <- lapply(seq_len(v), function(i) list(analysis = which(fold_id != i), assessment = which(fold_id == i)))
  structure(list(folds = out, fold_id = fold_id, group = group, v = v), class = "eye_grouped_folds")
}

#' Evaluate a model with grouped cross-validation
#' @param data Data frame.
#' @param formula Model formula.
#' @param family GLM family.
#' @param group Grouping columns.
#' @param v Number of folds.
#' @param metric Metric: log loss, Brier score, or accuracy.
#' @param seed Random seed.
#' @return An `eye_grouped_cv` object.
#' @export
grouped_cv <- function(
    data, formula, family = stats::binomial(), group = "participant_id",
    v = 5L, metric = c("log_loss", "brier", "accuracy"), seed = 1L) {
  metric <- match.arg(metric)
  folds <- grouped_folds(data, group, v, seed)
  response <- all.vars(formula)[1L]
  .assert_columns(data, response)
  scores <- lapply(seq_along(folds$folds), function(i) {
    f <- folds$folds[[i]]
    tryCatch({
      fit <- stats::glm(formula, data = data[f$analysis, , drop = FALSE], family = family)
      pred <- stats::predict(fit, newdata = data[f$assessment, , drop = FALSE], type = "response")
      y <- data[[response]][f$assessment]
      score <- switch(metric,
        log_loss = -mean(y * log(pmax(pred, 1e-12)) + (1 - y) * log(pmax(1 - pred, 1e-12)), na.rm = TRUE),
        brier = mean((y - pred)^2, na.rm = TRUE),
        accuracy = mean((pred >= 0.5) == y, na.rm = TRUE)
      )
      data.frame(fold = i, n_analysis = length(f$analysis), n_assessment = length(f$assessment), score = score, error = NA_character_)
    }, error = function(e) data.frame(
      fold = i, n_analysis = length(f$analysis), n_assessment = length(f$assessment),
      score = NA_real_, error = conditionMessage(e), stringsAsFactors = FALSE
    ))
  })
  structure(list(results = do.call(rbind, scores), folds = folds, metric = metric, formula = formula), class = "eye_grouped_cv")
}

#' @export
print.eye_grouped_cv <- function(x, ...) {
  cat("Grouped cross-validation\n")
  cat("Metric: ", x$metric, "\n", sep = "")
  print(x$results, row.names = FALSE)
  cat("Mean: ", mean(x$results$score), "\n", sep = "")
  invisible(x)
}

#' @export
plot.eye_grouped_cv <- function(x, ...) {
  graphics::plot(x$results$fold, x$results$score, type = "b", xlab = "Fold", ylab = x$metric, main = "Grouped cross-validation", ...)
  graphics::abline(h = mean(x$results$score), lty = 2)
  invisible(x$results)
}


#' Create cross-classified grouped folds
#'
#' Holds out levels from every declared grouping dimension simultaneously. The
#' assessment set is the intersection of held-out levels; the analysis set
#' excludes every held-out level. Rows combining held-out and retained levels
#' form a buffer and are deliberately used in neither set.
#'
#' @param data Data frame.
#' @param groups Two or more crossed grouping columns.
#' @param v Number of folds.
#' @param seed Random seed.
#' @return An `eye_crossed_grouped_folds` object.
#' @export
crossed_grouped_folds <- function(
    data,
    groups = c("participant_id", "item_id"),
    v = 5L,
    seed = 1L) {
  .assert_data_frame(data, "data")
  if (length(groups) < 2L || anyNA(groups) || any(!nzchar(groups))) {
    .eye_stop("`groups` must contain at least two non-empty crossed grouping columns.")
  }
  .assert_columns(data, groups)
  v <- as.integer(v)
  if (length(v) != 1L || is.na(v) || v < 2L) .eye_stop("`v` must be an integer of at least two.")
  set.seed(seed)
  assignments <- lapply(groups, function(group) {
    levels <- unique(as.character(data[[group]]))
    if (anyNA(levels) || any(!nzchar(levels))) .eye_stop("Crossed grouping columns cannot contain missing or empty values.")
    if (length(levels) < v) .eye_stop("Grouping column `", group, "` has fewer levels than requested folds.")
    fold <- sample(rep(seq_len(v), length.out = length(levels)))
    names(fold) <- levels
    unname(fold[as.character(data[[group]])])
  })
  names(assignments) <- groups
  folds <- lapply(seq_len(v), function(i) {
    held <- vapply(assignments, function(z) z == i, logical(nrow(data)))
    if (is.null(dim(held))) held <- matrix(held, ncol = length(assignments))
    assessment <- which(rowSums(held) == ncol(held))
    analysis <- which(rowSums(held) == 0L)
    buffer <- setdiff(seq_len(nrow(data)), c(analysis, assessment))
    list(analysis = analysis, assessment = assessment, buffer = buffer)
  })
  structure(
    list(folds = folds, assignments = assignments, groups = groups, v = v),
    class = "eye_crossed_grouped_folds"
  )
}

#' Cross-classified grouped cross-validation
#'
#' @param data Data frame.
#' @param formula Model formula.
#' @param family GLM family.
#' @param groups Crossed grouping columns.
#' @param v Number of folds.
#' @param metric Metric: log loss, Brier score, or accuracy.
#' @param seed Random seed.
#' @return An `eye_crossed_grouped_cv` object.
#' @export
crossed_grouped_cv <- function(
    data,
    formula,
    family = stats::binomial(),
    groups = c("participant_id", "item_id"),
    v = 5L,
    metric = c("log_loss", "brier", "accuracy"),
    seed = 1L) {
  metric <- match.arg(metric)
  folds <- crossed_grouped_folds(data, groups = groups, v = v, seed = seed)
  response <- all.vars(formula)[1L]
  scores <- lapply(seq_along(folds$folds), function(i) {
    f <- folds$folds[[i]]
    if (!length(f$analysis) || !length(f$assessment)) {
      return(data.frame(
        fold = i, n_analysis = length(f$analysis), n_assessment = length(f$assessment),
        n_buffer = length(f$buffer), score = NA_real_, error = "empty analysis or assessment set"
      ))
    }
    result <- tryCatch({
      fit <- stats::glm(formula, data = data[f$analysis, , drop = FALSE], family = family)
      pred <- stats::predict(fit, newdata = data[f$assessment, , drop = FALSE], type = "response")
      y <- data[[response]][f$assessment]
      score <- switch(metric,
        log_loss = -mean(y * log(pmax(pred, 1e-12)) + (1 - y) * log(pmax(1 - pred, 1e-12)), na.rm = TRUE),
        brier = mean((y - pred)^2, na.rm = TRUE),
        accuracy = mean((pred >= 0.5) == y, na.rm = TRUE)
      )
      data.frame(
        fold = i, n_analysis = length(f$analysis), n_assessment = length(f$assessment),
        n_buffer = length(f$buffer), score = score, error = NA_character_
      )
    }, error = function(e) data.frame(
      fold = i, n_analysis = length(f$analysis), n_assessment = length(f$assessment),
      n_buffer = length(f$buffer), score = NA_real_, error = conditionMessage(e)
    ))
    result
  })
  structure(
    list(results = do.call(rbind, scores), folds = folds, metric = metric, formula = formula),
    class = "eye_crossed_grouped_cv"
  )
}

#' @export
print.eye_crossed_grouped_cv <- function(x, ...) {
  cat("Cross-classified grouped cross-validation\n")
  cat("Metric: ", x$metric, "\n", sep = "")
  print(x$results, row.names = FALSE)
  cat("Mean: ", mean(x$results$score, na.rm = TRUE), "\n", sep = "")
  invisible(x)
}

#' @export
plot.eye_crossed_grouped_cv <- function(x, ...) {
  graphics::plot(
    x$results$fold, x$results$score, type = "b", xlab = "Fold", ylab = x$metric,
    main = "Cross-classified grouped cross-validation", ...
  )
  graphics::abline(h = mean(x$results$score, na.rm = TRUE), lty = 2)
  invisible(x$results)
}

#' Quantify leakage from row-wise rather than grouped validation
#' @param data Data frame.
#' @param formula Binary-outcome formula.
#' @param group Grouping columns.
#' @param v Number of folds.
#' @param seed Random seed.
#' @return Comparison table.
#' @export
quantify_process_leakage <- function(data, formula, group = c("participant_id", "item_id"), v = 5L, seed = 1L) {
  .assert_data_frame(data, "data")
  .assert_columns(data, group)
  row_id <- data.frame(.row_group = seq_len(nrow(data)), data, check.names = FALSE)
  jobs <- list(
    row_wise = function() grouped_cv(row_id, formula, group = ".row_group", v = v, metric = "log_loss", seed = seed),
    combined_group = function() grouped_cv(data, formula, group = group, v = v, metric = "log_loss", seed = seed)
  )
  for (nm in group) {
    jobs[[paste0("held_", nm)]] <- local({
      group_name <- nm
      function() grouped_cv(data, formula, group = group_name, v = v, metric = "log_loss", seed = seed)
    })
  }
  if (length(group) >= 2L) {
    jobs$cross_classified <- function() crossed_grouped_cv(
      data, formula, groups = group, v = v, metric = "log_loss", seed = seed
    )
  }
  rows <- lapply(names(jobs), function(nm) {
    result <- tryCatch(jobs[[nm]](), error = identity)
    if (inherits(result, "error")) {
      return(data.frame(scheme = nm, folds = 0L, successful_folds = 0L, mean_log_loss = NA_real_, error = conditionMessage(result)))
    }
    scores <- result$results$score
    data.frame(
      scheme = nm,
      folds = length(scores),
      successful_folds = sum(is.finite(scores)),
      mean_log_loss = if (any(is.finite(scores))) mean(scores[is.finite(scores)]) else NA_real_,
      error = NA_character_,
      stringsAsFactors = FALSE
    )
  })
  out <- .ep_bind_rows(rows)
  row_reference <- out$mean_log_loss[out$scheme == "row_wise"]
  out$optimistic_difference <- if (length(row_reference) && is.finite(row_reference[[1L]])) {
    out$mean_log_loss - row_reference[[1L]]
  } else {
    NA_real_
  }
  out
}

#' Run a preprocessing or AOI multiverse
#' @param x Input object.
#' @param specifications Named list of specification objects.
#' @param transform Function receiving `x` and one specification.
#' @param analyse Analysis function receiving the transformed object.
#' @param extract Function converting an analysis result to a data frame.
#' @return An `eye_multiverse` object.
#' @export
preprocessing_multiverse <- function(x, specifications, transform, analyse, extract = function(z) as.data.frame(z)) {
  if (!is.list(specifications) || !length(specifications)) .eye_stop("`specifications` must be a non-empty list.")
  if (is.null(names(specifications))) names(specifications) <- paste0("spec_", seq_along(specifications))
  rows <- lapply(names(specifications), function(nm) {
    result <- tryCatch({
      transformed <- transform(x, specifications[[nm]])
      fit <- analyse(transformed)
      d <- extract(fit)
      d$specification <- nm; d$error <- NA_character_; d
    }, error = function(e) data.frame(specification = nm, error = conditionMessage(e), stringsAsFactors = FALSE))
    result
  })
  structure(list(results = .ep_bind_rows(rows), specifications = specifications), class = "eye_multiverse")
}

#' @export
print.eye_multiverse <- function(x, ...) {
  cat("Preprocessing/AOI multiverse\n")
  cat("Specifications: ", length(x$specifications), "\n", sep = "")
  print(x$results, row.names = FALSE)
  invisible(x)
}

#' @export
plot.eye_multiverse <- function(x, estimate = "estimate", ...) {
  if (!estimate %in% names(x$results)) .eye_stop("Estimate column is absent: ", estimate)
  values <- x$results[[estimate]]; names(values) <- x$results$specification
  graphics::dotchart(values, xlab = estimate, main = "Multiverse estimates", ...)
  invisible(values)
}

#' Benchmark an eyeprocess operation
#' @param expr Function with no arguments to benchmark.
#' @param iterations Number of iterations.
#' @param label Benchmark label.
#' @return An `eye_benchmark` data frame.
#' @export
benchmark_eyeprocess <- function(expr, iterations = 5L, label = "operation") {
  if (!is.function(expr)) .eye_stop("`expr` must be a function with no required arguments.")
  iterations <- as.integer(iterations)
  if (length(iterations) != 1L || is.na(iterations) || iterations < 1L) .eye_stop("`iterations` must be a positive integer.")
  rows <- lapply(seq_len(iterations), function(i) {
    gc(reset = TRUE)
    start <- proc.time()[["elapsed"]]
    result <- expr()
    elapsed <- proc.time()[["elapsed"]] - start
    data.frame(label = label, iteration = i, elapsed_seconds = elapsed, result_size_bytes = as.numeric(object.size(result)), stringsAsFactors = FALSE)
  })
  structure(do.call(rbind, rows), class = c("eye_benchmark", "data.frame"))
}

#' @export
plot.eye_benchmark <- function(x, ...) {
  graphics::boxplot(elapsed_seconds ~ label, data = x, ylab = "Seconds", main = "eyeprocess benchmark", ...)
  invisible(x)
}

#' Audit reporting-guideline coverage
#' @param x An `eye_dataset`.
#' @param model Optional `eyeprocess_model`.
#' @param sensitivity Optional `eye_multiverse` sensitivity result.
#' @return An `eye_reporting_audit` data frame.
#' @export
reporting_guideline_audit <- function(x, model = NULL, sensitivity = NULL) {
  .assert_eye_dataset(x)
  checks <- data.frame(
    section = c("hardware", "sampling", "calibration", "coordinates", "preprocessing", "quality", "aois", "exclusions", "provenance", "model", "sensitivity", "interpretation"),
    item = c("device and software metadata", "sampling frequency", "calibration evidence", "coordinate space", "processing parameters", "quality metrics", "AOI definitions", "exclusion evidence", "transformation provenance", "model specification", "preprocessing and AOI sensitivity", "construct caution"),
    covered = c(
      nrow(x$recordings) > 0 && any(!is.na(x$recordings$device_model)),
      nrow(x$streams) > 0 && any(is.finite(x$streams$observed_rate_hz) | is.finite(x$streams$nominal_rate_hz)),
      nrow(x$calibrations) > 0,
      nrow(x$coordinate_spaces) > 0,
      nrow(x$provenance) > 0,
      nrow(x$quality) > 0,
      nrow(x$aoi_definitions) > 0,
      nrow(x$quality) > 0 && any(grepl("excl|invalid|missing", x$quality$metric, ignore.case = TRUE)),
      nrow(x$provenance) > 0,
      !is.null(model),
      inherits(sensitivity, "eye_multiverse"),
      TRUE
    ),
    stringsAsFactors = FALSE
  )
  checks$status <- ifelse(checks$covered, "pass", "missing")
  structure(checks, class = c("eye_reporting_audit", "data.frame"))
}

#' @export
print.eye_reporting_audit <- function(x, ...) {
  cat("Eye-tracking reporting-guideline audit\n")
  print.data.frame(x, row.names = FALSE)
  invisible(x)
}

#' @export
plot.eye_reporting_audit <- function(x, ...) {
  values <- as.integer(x$covered)
  names(values) <- x$section
  graphics::barplot(values, las = 2, ylim = c(0, 1), ylab = "Covered", main = "Reporting-guideline coverage", ...)
  invisible(values)
}

#' Write a reporting-guideline audit report
#' @param x An `eye_reporting_audit` object.
#' @param path Markdown output file.
#' @return Normalized report path.
#' @export
write_reporting_guideline_report <- function(x, path) {
  if (!inherits(x, "eye_reporting_audit")) .eye_stop("Expected an `eye_reporting_audit` object.")
  lines <- c(
    "# Eye-tracking reporting-guideline audit", "",
    paste0("Generated: ", format(Sys.time(), tz = "UTC", usetz = TRUE)), "",
    "| Section | Item | Covered | Status |", "|---|---|---|---|",
    vapply(seq_len(nrow(x)), function(i) sprintf(
      "| %s | %s | %s | %s |", x$section[i], x$item[i], x$covered[i], x$status[i]
    ), character(1))
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path, useBytes = TRUE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

#' Create a public, de-identified benchmark bundle
#' @param x An `eye_dataset`.
#' @param path Output directory.
#' @param max_participants Optional participant cap.
#' @param include_samples Whether to include sample-level tables.
#' @param overwrite Whether to replace the output directory.
#' @return Output directory.
#' @export
create_public_benchmark <- function(x, path, max_participants = 50L, include_samples = FALSE, overwrite = FALSE) {
  .assert_eye_dataset(x)
  max_participants <- as.integer(max_participants)
  if (length(max_participants) != 1L || is.na(max_participants) || max_participants < 1L) {
    .eye_stop("`max_participants` must be a positive integer.")
  }
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (dir.exists(path) && !overwrite) .eye_stop("Benchmark directory already exists: ", path)
  if (dir.exists(path)) unlink(path, recursive = TRUE, force = TRUE)
  y <- anonymize_eye_dataset(x, drop_raw = TRUE, retain_map = FALSE)
  participants <- unique(y$recordings$participant_id)
  if (length(participants) > max_participants) {
    keep <- participants[seq_len(max_participants)]
    recs <- y$recordings$recording_id[y$recordings$participant_id %in% keep]
    for (nm in canonical_table_names()) {
      d <- y[[nm]]
      if ("recording_id" %in% names(d)) y[[nm]] <- d[d$recording_id %in% recs, , drop = FALSE]
    }
  }
  if (!include_samples) {
    y$gaze_samples <- empty_eye_table("gaze_samples")
    y$eye_samples <- empty_eye_table("eye_samples")
    y$biometrics <- empty_eye_table("biometrics")
  }
  export_canonical(y, path, overwrite = TRUE, include_raw = FALSE)
  checks <- reporting_guideline_audit(y)
  utils::write.csv(checks, file.path(path, "reporting-guideline-audit.csv"), row.names = FALSE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

#' Write a methodological software-paper scaffold
#' @param path Output R Markdown file.
#' @param title Paper title.
#' @param author Author string.
#' @return The normalized path.
#' @export
write_software_paper_scaffold <- function(path, title = "eyeprocess: Reproducible Psychometric Process Modelling in R", author = "Stefanos Balaskas") {
  lines <- c(
    "---", paste0("title: \"", title, "\""), paste0("author: \"", author, "\""), "output: html_document", "---", "",
    "# Summary", "", "Describe the bounded contribution and non-goals.", "",
    "# Statement of need", "", "Document the fragmented eye-tracking, pupillometry, response-time, sequence, and IRT ecosystem.", "",
    "# Canonical data and provenance model", "", "Describe persons, recordings, trials, responses, samples, events, AOIs, features, quality, and provenance.", "",
    "# Interoperability", "", "Report vendor corpora, package adapters, Eye-Tracking-BIDS, and storage benchmarks.", "",
    "# Statistical models", "", "Separate descriptive process evidence, explanatory IRT, joint models, strategy models, dynamic models, and diffusion models.", "",
    "# Validation programme", "", "Report parameter recovery, interval coverage, calibration, misspecification, grouped validation, leakage, multiverse, and empirical reproduction.", "",
    "# Empirical demonstrations", "", "Include the real Gazepoint workflow and independent multi-vendor cases. Add the Raven reproduction only after data/licensing review.", "",
    "# Limitations and responsible interpretation", "", "Do not equate gaze or pupil variables with psychological constructs without validation.", "",
    "# Availability and reproducibility", "", "Provide package version, source tag, session information, manifests, and public benchmark bundles."
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path, useBytes = TRUE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

#' Run the complete validation-release programme
#'
#' @param corpus Validation corpus or manifest.
#' @param output_dir Output directory.
#' @param model_jobs Named list of model-validation job specifications. Each job
#'   supplies `simulator`, `fitter`, `extractor`, `truth_extractor`, and optional
#'   `grid` and `spec`.
#' @param sbc_jobs Named list of simulation-based-calibration job specifications.
#' @param engine_jobs Named list of equivalent-engine comparison jobs.
#' @param reproduction_jobs Named list of licensed empirical-reproduction jobs.
#' @param grouped_jobs Named list of grouped-validation jobs. Set `crossed = TRUE`
#'   in a job to call `crossed_grouped_cv()`.
#' @param leakage_jobs Named list of leakage-quantification jobs.
#' @param multiverse_jobs Named list of preprocessing-multiverse jobs.
#' @param benchmark_jobs Named list of zero-argument functions or benchmark argument lists.
#' @param reporting_dataset Optional `eye_dataset` for reporting-guideline coverage.
#' @param public_benchmark_dataset Optional `eye_dataset` from which to write a
#'   de-identified public benchmark bundle.
#' @param public_benchmark_include_samples Whether the public benchmark retains
#'   sample-level tables.
#' @param advanced_evidence Optional named evidence records keyed by model function.
#' @param evidence_spec Advanced-model promotion-gate specification.
#' @param overwrite Whether to replace the output directory.
#' @return An `eye_validation_program` object.
#' @export
run_eyeprocess_validation_program <- function(
    corpus,
    output_dir,
    model_jobs = list(),
    sbc_jobs = list(),
    engine_jobs = list(),
    reproduction_jobs = list(),
    grouped_jobs = list(),
    leakage_jobs = list(),
    multiverse_jobs = list(),
    benchmark_jobs = list(),
    reporting_dataset = NULL,
    public_benchmark_dataset = NULL,
    public_benchmark_include_samples = FALSE,
    advanced_evidence = list(),
    evidence_spec = advanced_model_evidence_spec(),
    overwrite = FALSE) {
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  if (dir.exists(output_dir) && !overwrite) .eye_stop("Validation output already exists: ", output_dir)
  if (dir.exists(output_dir)) unlink(output_dir, recursive = TRUE, force = TRUE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  plot_dir <- file.path(output_dir, "plots")
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

  .job_names <- function(x, prefix) {
    if (!length(x)) return(character())
    nm <- names(x)
    if (is.null(nm)) nm <- rep("", length(x))
    missing <- is.na(nm) | !nzchar(nm)
    nm[missing] <- paste0(prefix, which(missing))
    make.unique(nm)
  }
  .save_plot <- function(path, expr, width = 1200, height = 900, res = 140) {
    grDevices::png(path, width = width, height = height, res = res)
    on.exit(grDevices::dev.off(), add = TRUE)
    force(expr)
    invisible(path)
  }
  .merge_evidence <- function(evidence, name, component, value) {
    if (is.null(value) || !nzchar(name)) return(evidence)
    record <- evidence[[name]] %||% list()
    if (is.null(record[[component]])) record[[component]] <- value
    evidence[[name]] <- record
    evidence
  }

  corpus_result <- if (inherits(corpus, "eye_corpus_validation")) corpus else validate_eye_corpus(corpus)
  vendor_audit <- audit_vendor_validation(corpus_result)
  utils::write.csv(corpus_result$summary, file.path(output_dir, "corpus-summary.csv"), row.names = FALSE)
  utils::write.csv(vendor_audit, file.path(output_dir, "vendor-audit.csv"), row.names = FALSE)
  write_vendor_validation_report(vendor_audit, file.path(output_dir, "vendor-validation.md"))
  .save_plot(file.path(plot_dir, "vendor-pass-rate.png"), plot(vendor_audit, "pass_rate"))
  .save_plot(file.path(plot_dir, "vendor-case-counts.png"), plot(vendor_audit, "cases"))

  names(model_jobs) <- .job_names(model_jobs, "model-")
  model_results <- lapply(model_jobs, function(job) do.call(run_model_validation, job))
  for (nm in names(model_results)) {
    saveRDS(model_results[[nm]], file.path(output_dir, paste0("model-validation-", nm, ".rds")))
    utils::write.csv(model_validation_summary(model_results[[nm]]), file.path(output_dir, paste0("model-validation-", nm, ".csv")), row.names = FALSE)
    if (any(model_results[[nm]]$runs$converged, na.rm = TRUE)) {
      .save_plot(file.path(plot_dir, paste0("model-recovery-", nm, ".png")), plot(model_results[[nm]], "recovery"))
      model_summary <- model_validation_summary(model_results[[nm]])
      if (any(is.finite(model_summary$bias))) {
        .save_plot(file.path(plot_dir, paste0("model-bias-", nm, ".png")), plot(model_results[[nm]], "bias"))
      }
      if (any(is.finite(model_summary$coverage))) {
        .save_plot(file.path(plot_dir, paste0("model-coverage-", nm, ".png")), plot(model_results[[nm]], "coverage"))
      }
    }
    advanced_evidence <- .merge_evidence(advanced_evidence, nm, "recovery", model_results[[nm]])
  }

  names(sbc_jobs) <- .job_names(sbc_jobs, "sbc-")
  sbc_results <- lapply(sbc_jobs, function(job) do.call(simulation_based_calibration, job))
  for (nm in names(sbc_results)) {
    saveRDS(sbc_results[[nm]], file.path(output_dir, paste0("sbc-", nm, ".rds")))
    utils::write.csv(sbc_summary(sbc_results[[nm]]), file.path(output_dir, paste0("sbc-", nm, ".csv")), row.names = FALSE)
    parameters <- unique(stats::na.omit(sbc_results[[nm]]$ranks$parameter))
    for (parameter in parameters) {
      .save_plot(
        file.path(plot_dir, paste0("sbc-", nm, "-", .ep_sanitize_id(parameter, "parameter"), ".png")),
        plot(sbc_results[[nm]], parameter = parameter)
      )
    }
    advanced_evidence <- .merge_evidence(advanced_evidence, nm, "calibration", sbc_results[[nm]])
  }

  names(engine_jobs) <- .job_names(engine_jobs, "engine-")
  engine_results <- lapply(engine_jobs, function(job) do.call(compare_model_engines, job))
  for (nm in names(engine_results)) {
    saveRDS(engine_results[[nm]], file.path(output_dir, paste0("engine-comparison-", nm, ".rds")))
    utils::write.csv(engine_results[[nm]]$estimates, file.path(output_dir, paste0("engine-comparison-", nm, ".csv")), row.names = FALSE)
    parameters <- unique(stats::na.omit(engine_results[[nm]]$estimates$parameter))
    for (parameter in parameters) {
      .save_plot(
        file.path(plot_dir, paste0("engine-comparison-", nm, "-", .ep_sanitize_id(parameter, "parameter"), ".png")),
        plot(engine_results[[nm]], parameter = parameter)
      )
    }
    advanced_evidence <- .merge_evidence(advanced_evidence, nm, "engine_equivalence", engine_results[[nm]])
  }

  names(reproduction_jobs) <- .job_names(reproduction_jobs, "reproduction-")
  reproduction_results <- lapply(reproduction_jobs, function(job) do.call(run_raven_reproduction, job))
  for (nm in names(reproduction_results)) {
    saveRDS(reproduction_results[[nm]], file.path(output_dir, paste0("empirical-reproduction-", nm, ".rds")))
    utils::write.csv(reproduction_results[[nm]]$comparison, file.path(output_dir, paste0("empirical-reproduction-", nm, ".csv")), row.names = FALSE)
    if (all(c("target", "estimate") %in% names(reproduction_results[[nm]]$comparison))) {
      .save_plot(file.path(plot_dir, paste0("empirical-reproduction-", nm, ".png")), plot(reproduction_results[[nm]]))
    }
    advanced_evidence <- .merge_evidence(advanced_evidence, nm, "empirical_reproduction", reproduction_results[[nm]])
  }

  names(grouped_jobs) <- .job_names(grouped_jobs, "grouped-")
  grouped_results <- lapply(grouped_jobs, function(job) {
    crossed <- isTRUE(job$crossed)
    job$crossed <- NULL
    if (crossed) do.call(crossed_grouped_cv, job) else do.call(grouped_cv, job)
  })
  for (nm in names(grouped_results)) {
    saveRDS(grouped_results[[nm]], file.path(output_dir, paste0("grouped-validation-", nm, ".rds")))
    utils::write.csv(grouped_results[[nm]]$results, file.path(output_dir, paste0("grouped-validation-", nm, ".csv")), row.names = FALSE)
    .save_plot(file.path(plot_dir, paste0("grouped-validation-", nm, ".png")), plot(grouped_results[[nm]]))
    advanced_evidence <- .merge_evidence(advanced_evidence, nm, "grouped_validation", grouped_results[[nm]])
  }

  names(leakage_jobs) <- .job_names(leakage_jobs, "leakage-")
  leakage_results <- lapply(leakage_jobs, function(job) do.call(quantify_process_leakage, job))
  for (nm in names(leakage_results)) {
    utils::write.csv(leakage_results[[nm]], file.path(output_dir, paste0("leakage-", nm, ".csv")), row.names = FALSE)
    values <- leakage_results[[nm]]$mean_log_loss
    names(values) <- leakage_results[[nm]]$scheme
    .save_plot(
      file.path(plot_dir, paste0("leakage-", nm, ".png")),
      graphics::barplot(values, ylab = "Mean log loss", main = "Grouped versus row-wise validation")
    )
  }

  names(multiverse_jobs) <- .job_names(multiverse_jobs, "multiverse-")
  multiverse_results <- lapply(multiverse_jobs, function(job) do.call(preprocessing_multiverse, job))
  for (nm in names(multiverse_results)) {
    saveRDS(multiverse_results[[nm]], file.path(output_dir, paste0("multiverse-", nm, ".rds")))
    utils::write.csv(multiverse_results[[nm]]$results, file.path(output_dir, paste0("multiverse-", nm, ".csv")), row.names = FALSE)
    numeric_columns <- names(multiverse_results[[nm]]$results)[vapply(multiverse_results[[nm]]$results, is.numeric, logical(1))]
    numeric_columns <- setdiff(numeric_columns, "specification")
    if (length(numeric_columns)) {
      .save_plot(file.path(plot_dir, paste0("multiverse-", nm, ".png")), plot(multiverse_results[[nm]], estimate = numeric_columns[[1L]]))
    }
    advanced_evidence <- .merge_evidence(advanced_evidence, nm, "sensitivity", multiverse_results[[nm]])
  }

  names(benchmark_jobs) <- .job_names(benchmark_jobs, "benchmark-")
  benchmarks <- lapply(names(benchmark_jobs), function(nm) {
    job <- benchmark_jobs[[nm]]
    if (is.function(job)) benchmark_eyeprocess(job, label = nm) else do.call(benchmark_eyeprocess, c(job, list(label = nm)))
  })
  names(benchmarks) <- names(benchmark_jobs)
  if (length(benchmarks)) {
    benchmark_table <- .ep_bind_rows(benchmarks)
    class(benchmark_table) <- c("eye_benchmark", "data.frame")
    utils::write.csv(benchmark_table, file.path(output_dir, "benchmarks.csv"), row.names = FALSE)
    .save_plot(file.path(plot_dir, "benchmarks.png"), plot(benchmark_table))
  }

  reporting <- NULL
  if (!is.null(reporting_dataset)) {
    reporting_sensitivity <- if (length(multiverse_results)) multiverse_results[[1L]] else NULL
    reporting <- reporting_guideline_audit(reporting_dataset, sensitivity = reporting_sensitivity)
    utils::write.csv(reporting, file.path(output_dir, "reporting-guideline-audit.csv"), row.names = FALSE)
    write_reporting_guideline_report(reporting, file.path(output_dir, "reporting-guideline-audit.md"))
    .save_plot(file.path(plot_dir, "reporting-guideline-coverage.png"), plot(reporting))
  }

  public_benchmark <- NULL
  if (!is.null(public_benchmark_dataset)) {
    public_benchmark <- create_public_benchmark(
      public_benchmark_dataset,
      file.path(output_dir, "public-benchmark"),
      include_samples = public_benchmark_include_samples,
      overwrite = TRUE
    )
  }
  paper_scaffold <- write_software_paper_scaffold(file.path(output_dir, "eyeprocess-software-paper.Rmd"))

  evidence_audit <- audit_advanced_model_evidence(advanced_evidence, evidence_spec)
  utils::write.csv(evidence_audit, file.path(output_dir, "advanced-model-evidence.csv"), row.names = FALSE)
  write_advanced_model_evidence_report(evidence_audit, file.path(output_dir, "advanced-model-evidence.md"))
  .save_plot(file.path(plot_dir, "advanced-model-evidence.png"), plot(evidence_audit))

  out <- list(
    corpus = corpus_result, vendor_audit = vendor_audit, models = model_results,
    sbc = sbc_results, engine_comparisons = engine_results,
    reproductions = reproduction_results, grouped_validation = grouped_results,
    leakage = leakage_results, multiverses = multiverse_results,
    benchmarks = benchmarks, reporting = reporting, public_benchmark = public_benchmark,
    paper_scaffold = paper_scaffold, advanced_evidence = advanced_evidence,
    evidence_audit = evidence_audit, output_dir = output_dir
  )
  class(out) <- "eye_validation_program"
  saveRDS(out, file.path(output_dir, "validation-program.rds"))
  out
}

#' @export
print.eye_validation_program <- function(x, ...) {
  cat("eyeprocess validation-release programme\n")
  cat("Corpus status: ", toupper(x$corpus$status), "\n", sep = "")
  cat("Model jobs:    ", length(x$models), "\n", sep = "")
  cat("SBC jobs:      ", length(x$sbc), "\n", sep = "")
  cat("Engine jobs:   ", length(x$engine_comparisons), "\n", sep = "")
  cat("Reproductions: ", length(x$reproductions), "\n", sep = "")
  cat("Grouped jobs:  ", length(x$grouped_validation), "\n", sep = "")
  cat("Leakage jobs:  ", length(x$leakage), "\n", sep = "")
  cat("Multiverses:   ", length(x$multiverses), "\n", sep = "")
  cat("Benchmarks:    ", length(x$benchmarks), "\n", sep = "")
  cat("Evidence pass: ", sum(x$evidence_audit$status == "pass"), "/", nrow(x$evidence_audit), "\n", sep = "")
  cat("Output:        ", x$output_dir, "\n", sep = "")
  invisible(x)
}
