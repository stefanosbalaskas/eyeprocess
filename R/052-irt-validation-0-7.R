# eyeprocess 0.7.0.9000 ------------------------------------------------------
# Unified validation contracts for multimodal/process IRT.
#
# This file is deliberately estimator-agnostic.  Exact scientific claims must
# come from a generator/fitter/extractor combination appropriate to the model.
# The functions below standardise evidence, failure reporting, calibration,
# misspecification stress tests, and grouped/external transport validation.

.ep07_v_as_df <- function(x) {
  if (is.data.frame(x)) return(x)
  as.data.frame(x, stringsAsFactors = FALSE)
}

.ep07_v_require <- function(x, cols, arg = deparse(substitute(x))) {
  miss <- setdiff(cols, names(x))
  if (length(miss)) {
    stop(arg, " is missing required column(s): ", paste(miss, collapse = ", "),
         call. = FALSE)
  }
  invisible(TRUE)
}

.ep07_v_num <- function(x) suppressWarnings(as.numeric(x))

.ep07_v_ci <- function(x, level = 0.95) {
  x <- x[is.finite(x)]
  if (!length(x)) return(c(lower = NA_real_, upper = NA_real_))
  alpha <- (1 - level) / 2
  stats::quantile(x, probs = c(alpha, 1 - alpha), na.rm = TRUE,
                  names = FALSE, type = 8) |>
    stats::setNames(c("lower", "upper"))
}

.ep07_v_group_key <- function(d, by) {
  if (!length(by)) return(rep("all", nrow(d)))
  do.call(paste, c(d[by], sep = "\r"))
}

.ep07_v_bind <- function(xs) {
  xs <- Filter(function(x) !is.null(x) && nrow(x) > 0L, xs)
  if (!length(xs)) return(data.frame())
  cols <- unique(unlist(lapply(xs, names), use.names = FALSE))
  xs <- lapply(xs, function(x) {
    for (nm in setdiff(cols, names(x))) x[[nm]] <- NA
    x[cols]
  })
  do.call(rbind, xs)
}

#' Specify a validation programme for a process-IRT model
#'
#' Creates an evidence contract rather than executing a particular estimator.
#' The object can be passed to stress-test and evidence-grading helpers.
#'
#' @param model_id Stable model identifier.
#' @param replications Number of simulation replications planned.
#' @param parameters Parameter families expected to be recovered.
#' @param metrics Recovery metrics to require.
#' @param grouped_validation Grouping variables for transport validation.
#' @param preprocessing_variants Optional named preprocessing variants.
#' @param misspecification_scenarios Optional named scenarios.
#' @param thresholds Named evidence thresholds.
#' @param seed Reproducibility seed.
#' @param notes Free-text scientific notes.
#' @export
irt_validation_spec <- function(
    model_id,
    replications = 250L,
    parameters = NULL,
    metrics = c("bias", "rmse", "coverage", "interval_width", "convergence"),
    grouped_validation = c("device", "session", "site"),
    preprocessing_variants = NULL,
    misspecification_scenarios = NULL,
    thresholds = list(
      max_abs_bias = 0.10,
      max_rmse = 0.30,
      min_coverage = 0.90,
      max_failure_rate = 0.05,
      min_external_folds = 2L
    ),
    seed = 20260808L,
    notes = NULL) {
  if (!is.character(model_id) || length(model_id) != 1L || !nzchar(model_id))
    stop("model_id must be one non-empty string.", call. = FALSE)
  replications <- as.integer(replications)
  if (!is.finite(replications) || replications < 1L)
    stop("replications must be >= 1.", call. = FALSE)

  structure(list(
    model_id = model_id,
    replications = replications,
    parameters = parameters,
    metrics = unique(metrics),
    grouped_validation = unique(grouped_validation),
    preprocessing_variants = preprocessing_variants,
    misspecification_scenarios = misspecification_scenarios,
    thresholds = thresholds,
    seed = as.integer(seed),
    notes = notes,
    contract_version = "0.7.0",
    created_with = "eyeprocess"
  ), class = "eye_irt_validation_spec")
}

#' Canonicalise parameter-recovery results
#'
#' @param results Data frame with at least `replicate`, `parameter`, `truth`,
#'   and `estimate`; optional `lower`, `upper`, `converged`, `scenario`,
#'   `engine`, and `failure_type` columns are retained.
#' @export
as_irt_recovery_results <- function(results) {
  d <- .ep07_v_as_df(results)
  .ep07_v_require(d, c("replicate", "parameter", "truth", "estimate"), "results")
  d$truth <- .ep07_v_num(d$truth)
  d$estimate <- .ep07_v_num(d$estimate)
  if (!"converged" %in% names(d)) d$converged <- is.finite(d$estimate)
  d$converged <- as.logical(d$converged)
  if (!"failure_type" %in% names(d)) d$failure_type <- NA_character_
  if (!"scenario" %in% names(d)) d$scenario <- "baseline"
  if (!"engine" %in% names(d)) d$engine <- "unspecified"
  d$error <- d$estimate - d$truth
  class(d) <- unique(c("eye_irt_recovery_results", class(d)))
  d
}

#' Summarise parameter recovery
#'
#' @param results Canonical or raw recovery results.
#' @param by Grouping columns.
#' @param interval_level Nominal interval level, used only for labelling.
#' @export
summarize_parameter_recovery <- function(
    results,
    by = c("scenario", "engine", "parameter"),
    interval_level = 0.95) {
  d <- as_irt_recovery_results(results)
  by <- intersect(by, names(d))
  keys <- .ep07_v_group_key(d, by)
  sp <- split(seq_len(nrow(d)), keys)

  out <- lapply(sp, function(idx) {
    z <- d[idx, , drop = FALSE]
    ok <- isTRUE(z$converged) | (!is.na(z$converged) & z$converged)
    finite <- ok & is.finite(z$truth) & is.finite(z$estimate)
    err <- z$error[finite]
    cover <- rep(NA, nrow(z))
    if (all(c("lower", "upper") %in% names(z))) {
      cover <- z$lower <= z$truth & z$truth <= z$upper
      width <- z$upper - z$lower
    } else {
      width <- rep(NA_real_, nrow(z))
    }
    row <- if (length(by)) z[1L, by, drop = FALSE] else data.frame()
    cbind(row, data.frame(
      n = nrow(z),
      n_converged = sum(ok, na.rm = TRUE),
      convergence_rate = mean(ok, na.rm = TRUE),
      failure_rate = mean(!ok, na.rm = TRUE),
      bias = if (length(err)) mean(err, na.rm = TRUE) else NA_real_,
      absolute_bias = if (length(err)) abs(mean(err, na.rm = TRUE)) else NA_real_,
      rmse = if (length(err)) sqrt(mean(err^2, na.rm = TRUE)) else NA_real_,
      mae = if (length(err)) mean(abs(err), na.rm = TRUE) else NA_real_,
      coverage = if (any(!is.na(cover))) mean(cover, na.rm = TRUE) else NA_real_,
      interval_width = if (any(is.finite(width))) mean(width, na.rm = TRUE) else NA_real_,
      nominal_coverage = interval_level,
      stringsAsFactors = FALSE
    ))
  })
  out <- .ep07_v_bind(out)
  class(out) <- unique(c("eye_irt_recovery_summary", class(out)))
  out
}

#' Audit bias
#' @param results Validation or model results.
#' @param threshold Decision or diagnostic threshold.
#' @param by Grouping variables used when summarizing results.
#' @export
audit_bias <- function(results, threshold = 0.10, by = c("scenario", "engine", "parameter")) {
  s <- if (inherits(results, "eye_irt_recovery_summary")) results else
    summarize_parameter_recovery(results, by = by)
  s$threshold <- threshold
  s$pass <- is.finite(s$absolute_bias) & s$absolute_bias <= threshold
  structure(s, class = unique(c("eye_irt_bias_audit", class(s))))
}

#' Audit rmse
#' @param results Validation or model results.
#' @param threshold Decision or diagnostic threshold.
#' @param by Grouping variables used when summarizing results.
#' @export
audit_rmse <- function(results, threshold = 0.30, by = c("scenario", "engine", "parameter")) {
  s <- if (inherits(results, "eye_irt_recovery_summary")) results else
    summarize_parameter_recovery(results, by = by)
  s$threshold <- threshold
  s$pass <- is.finite(s$rmse) & s$rmse <= threshold
  structure(s, class = unique(c("eye_irt_rmse_audit", class(s))))
}

#' Audit coverage
#' @param results Validation or model results.
#' @param minimum Minimum acceptable value or threshold.
#' @param maximum Maximum acceptable value or threshold.
#' @param by Grouping variables used when summarizing results.
#' @export
audit_coverage <- function(results, minimum = 0.90, maximum = 1,
                           by = c("scenario", "engine", "parameter")) {
  s <- if (inherits(results, "eye_irt_recovery_summary")) results else
    summarize_parameter_recovery(results, by = by)
  s$minimum <- minimum
  s$maximum <- maximum
  s$pass <- is.finite(s$coverage) & s$coverage >= minimum & s$coverage <= maximum
  structure(s, class = unique(c("eye_irt_coverage_audit", class(s))))
}

#' Audit interval width
#' @param results Validation or model results.
#' @param maximum Maximum acceptable value or threshold.
#' @param by Grouping variables used when summarizing results.
#' @export
audit_interval_width <- function(results, maximum = Inf,
                                 by = c("scenario", "engine", "parameter")) {
  s <- if (inherits(results, "eye_irt_recovery_summary")) results else
    summarize_parameter_recovery(results, by = by)
  s$maximum <- maximum
  s$pass <- is.finite(s$interval_width) & s$interval_width <= maximum
  structure(s, class = unique(c("eye_irt_interval_width_audit", class(s))))
}

#' Audit convergence and classified failures
#' @param results Validation or model results.
#' @param minimum Minimum acceptable value or threshold.
#' @param by Grouping variables used when summarizing results.
#' @export
audit_convergence <- function(results, minimum = 0.95,
                              by = c("scenario", "engine")) {
  d <- as_irt_recovery_results(results)
  by <- intersect(by, names(d))
  key <- .ep07_v_group_key(d, by)
  sp <- split(seq_len(nrow(d)), key)
  out <- lapply(sp, function(idx) {
    z <- d[idx, , drop = FALSE]
    conv_by_rep <- tapply(z$converged, z$replicate, function(x) all(x, na.rm = TRUE))
    row <- if (length(by)) z[1L, by, drop = FALSE] else data.frame()
    cbind(row, data.frame(
      n_replicates = length(conv_by_rep),
      convergence_rate = mean(conv_by_rep, na.rm = TRUE),
      minimum = minimum,
      pass = mean(conv_by_rep, na.rm = TRUE) >= minimum,
      stringsAsFactors = FALSE
    ))
  })
  structure(.ep07_v_bind(out), class = c("eye_irt_convergence_audit", "data.frame"))
}

#' Classify common estimator failures without hiding the original message
#'
#' @param x Error/condition/message vector.
#' @export
validation_failure_taxonomy <- function(x) {
  msg <- if (inherits(x, "condition")) conditionMessage(x) else as.character(x)
  low <- tolower(msg)
  type <- rep("other", length(low))
  type[grepl("singular|boundary", low)] <- "singular_fit"
  type[grepl("converg|gradient|hessian", low)] <- "nonconvergence"
  type[grepl("divergent|treedepth|rhat|effective sample|ess", low)] <- "bayesian_sampling"
  type[grepl("identif|rank deficient|not positive definite|non-positive", low)] <- "identifiability"
  type[grepl("overflow|underflow|nan|infinite|non-finite", low)] <- "numerical"
  type[grepl("memory|cannot allocate", low)] <- "resource"
  type[grepl("package .* required|there is no package|not installed", low)] <- "dependency"
  data.frame(message = msg, failure_type = type, stringsAsFactors = FALSE)
}

#' Audit empirical identifiability from replicate estimates
#'
#' Flags parameters with near-zero estimate variance, explosive dispersion,
#' excessive missingness, or strongly correlated estimates when a covariance
#' matrix is supplied.
#' @param results Validation or model results.
#' @param max_missing Maximum acceptable missingness.
#' @param max_sd_ratio Maximum acceptable standard-deviation ratio.
#' @param correlation_matrix Optional parameter-correlation matrix.
#' @param max_abs_correlation Maximum acceptable absolute parameter correlation.
#' @export
audit_identifiability <- function(results, max_missing = 0.05,
                                  max_sd_ratio = 10,
                                  correlation_matrix = NULL,
                                  max_abs_correlation = 0.995) {
  d <- as_irt_recovery_results(results)
  sp <- split(d, d$parameter)
  rows <- lapply(names(sp), function(p) {
    z <- sp[[p]]
    te <- stats::sd(z$truth, na.rm = TRUE)
    ee <- stats::sd(z$estimate, na.rm = TRUE)
    miss <- mean(!is.finite(z$estimate))
    ratio <- if (is.finite(te) && te > 0) ee / te else NA_real_
    data.frame(parameter = p, missing_rate = miss,
               truth_sd = te, estimate_sd = ee, sd_ratio = ratio,
               pass = miss <= max_missing &&
                 (is.na(ratio) || (is.finite(ratio) && ratio <= max_sd_ratio && ratio > 1e-6)),
               stringsAsFactors = FALSE)
  })
  out <- .ep07_v_bind(rows)
  corr_issue <- FALSE
  corr_max <- NA_real_
  if (!is.null(correlation_matrix)) {
    C <- as.matrix(correlation_matrix)
    diag(C) <- NA_real_
    corr_max <- suppressWarnings(max(abs(C), na.rm = TRUE))
    if (!is.finite(corr_max)) corr_max <- NA_real_
    corr_issue <- is.finite(corr_max) && corr_max > max_abs_correlation
  }
  attr(out, "max_abs_parameter_correlation") <- corr_max
  attr(out, "correlation_issue") <- corr_issue
  if (corr_issue) out$pass <- FALSE
  structure(out, class = c("eye_irt_identifiability_audit", "data.frame"))
}

#' Monte Carlo standard errors for validation metrics
#' @param results Validation or model results.
#' @param metric Metric to calculate or audit.
#' @export
validation_mcse <- function(results, metric = c("bias", "rmse", "coverage")) {
  metric <- match.arg(metric)
  d <- as_irt_recovery_results(results)
  if (metric == "coverage") {
    if (!all(c("lower", "upper") %in% names(d)))
      stop("Coverage MCSE requires lower and upper interval columns.", call. = FALSE)
    x <- as.numeric(d$lower <= d$truth & d$truth <= d$upper)
    p <- mean(x, na.rm = TRUE); n <- sum(is.finite(x))
    return(data.frame(metric = metric, estimate = p,
                      mcse = sqrt(p * (1 - p) / n), n = n))
  }
  e <- d$estimate - d$truth
  e <- e[is.finite(e)]
  if (metric == "bias") {
    val <- mean(e); mcse <- stats::sd(e) / sqrt(length(e))
  } else {
    sq <- e^2; val <- sqrt(mean(sq))
    # Delta-method MCSE for sqrt(mean(e^2)).
    mcse <- if (val > 0) stats::sd(sq) / sqrt(length(sq)) / (2 * val) else 0
  }
  data.frame(metric = metric, estimate = val, mcse = mcse, n = length(e))
}

#' Approximate simulation replications needed for a target Monte Carlo error
#' @param target_mcse Target Monte Carlo standard error.
#' @param metric Metric to calculate or audit.
#' @param anticipated_sd Anticipated standard deviation.
#' @param anticipated_probability Anticipated probability for a binary metric.
#' @param minimum Minimum acceptable value or threshold.
#' @export
recommended_validation_replications <- function(
    target_mcse = 0.01,
    metric = c("coverage", "mean"),
    anticipated_sd = 1,
    anticipated_probability = 0.95,
    minimum = 100L) {
  metric <- match.arg(metric)
  if (metric == "coverage") {
    n <- anticipated_probability * (1 - anticipated_probability) / target_mcse^2
  } else {
    n <- anticipated_sd^2 / target_mcse^2
  }
  as.integer(max(minimum, ceiling(n)))
}

# Simulation-based calibration -------------------------------------------------

#' Run generic simulation-based calibration
#'
#' @param simulator Function `simulator(replicate)` returning `list(data, truth)`;
#'   `truth` must be a named numeric vector.
#' @param fitter Function `fitter(data)` returning a fitted object.
#' @param posterior_draws Function returning a draws matrix/data frame whose
#'   columns match names in `truth`.
#' @param replications Number of SBC replications.
#' @param seed RNG seed.
#' @export
run_sbc <- function(simulator, fitter, posterior_draws, replications = 100L,
                    seed = 20260808L) {
  stopifnot(is.function(simulator), is.function(fitter), is.function(posterior_draws))
  set.seed(seed)
  rows <- vector("list", replications)
  failures <- list()
  for (r in seq_len(replications)) {
    one <- tryCatch({
      s <- simulator(r)
      if (!is.list(s) || is.null(s$data) || is.null(s$truth))
        stop("simulator() must return list(data=..., truth=named_numeric).")
      truth <- unlist(s$truth)
      if (is.null(names(truth)) || any(!nzchar(names(truth))))
        stop("truth must be a named numeric vector.")
      fit <- fitter(s$data)
      dr <- as.matrix(posterior_draws(fit))
      keep <- intersect(names(truth), colnames(dr))
      if (!length(keep)) stop("No posterior-draw columns match truth names.")
      do.call(rbind, lapply(keep, function(p) {
        v <- dr[, p]
        v <- v[is.finite(v)]
        data.frame(replicate = r, parameter = p, truth = truth[[p]],
                   rank = sum(v < truth[[p]]), draws = length(v),
                   normalized_rank = (sum(v < truth[[p]]) + 0.5) / (length(v) + 1),
                   stringsAsFactors = FALSE)
      }))
    }, error = function(e) {
      failures[[length(failures) + 1L]] <<- cbind(
        data.frame(replicate = r, stringsAsFactors = FALSE),
        validation_failure_taxonomy(e)
      )
      NULL
    })
    rows[[r]] <- one
  }
  out <- structure(list(
    ranks = .ep07_v_bind(rows),
    failures = .ep07_v_bind(failures),
    replications = as.integer(replications),
    seed = seed,
    method = "prior_simulation_based_calibration"
  ), class = "eye_irt_sbc")
  out
}

#' Define a posterior-SBC replication contract
#'
#' Posterior SBC is deliberately callback-driven: conditioning/augmentation
#' differs by model, and eyeprocess does not pretend that re-running ordinary
#' prior SBC on an observed-data neighbourhood is posterior SBC.
#'
#' @param replication Function `(replicate, observed_data)` returning
#'   `list(truth=named_numeric, draws=matrix_or_data_frame)`.  The callback is
#'   responsible for the conditional posterior-SBC construction appropriate to
#'   the model, including fitting to the observed data and the required
#'   self-consistency experiment.
#' @export
posterior_sbc_contract <- function(replication) {
  if (!is.function(replication)) stop("replication must be a function.", call. = FALSE)
  structure(list(replication = replication,
                 requirement = paste(
                   "Each replication must be generated from a posterior-SBC",
                   "self-consistency experiment conditional on the observed data;",
                   "ordinary prior SBC is not an acceptable substitute.")),
            class = "eye_posterior_sbc_contract")
}

#' Run posterior simulation-based calibration from an explicit contract
#' @param observed_data Observed dataset used by the calibration procedure.
#' @param contract Posterior-SBC or validation contract.
#' @param replications Number of simulation or validation replications.
#' @param seed Random-number seed.
#' @export
run_posterior_sbc <- function(observed_data, contract, replications = 100L,
                              seed = 20260808L) {
  if (!inherits(contract, "eye_posterior_sbc_contract"))
    stop("contract must be created by posterior_sbc_contract().", call. = FALSE)
  set.seed(seed)
  rows <- vector("list", replications)
  failures <- list()
  for (r in seq_len(replications)) {
    one <- tryCatch({
      z <- contract$replication(r, observed_data)
      truth <- unlist(z$truth)
      dr <- as.matrix(z$draws)
      keep <- intersect(names(truth), colnames(dr))
      if (!length(keep)) stop("Posterior-SBC draws do not match named truth parameters.")
      do.call(rbind, lapply(keep, function(p) {
        v <- dr[, p]; v <- v[is.finite(v)]
        rk <- sum(v < truth[[p]])
        data.frame(replicate = r, parameter = p, truth = truth[[p]],
                   rank = rk, draws = length(v),
                   normalized_rank = (rk + 0.5) / (length(v) + 1),
                   stringsAsFactors = FALSE)
      }))
    }, error = function(e) {
      failures[[length(failures) + 1L]] <<- cbind(
        data.frame(replicate = r, stringsAsFactors = FALSE),
        validation_failure_taxonomy(e))
      NULL
    })
    rows[[r]] <- one
  }
  structure(list(ranks = .ep07_v_bind(rows), failures = .ep07_v_bind(failures),
                 replications = replications, seed = seed,
                 method = "posterior_simulation_based_calibration",
                 requirement = contract$requirement),
            class = c("eye_posterior_sbc", "eye_irt_sbc"))
}

#' Audit SBC rank uniformity
#'
#' Uses binned chi-square diagnostics as a coarse screening diagnostic and also
#' reports the mean/variance of normalized ranks. It is not a replacement for
#' rank-histogram inspection.
#' @param x Object to print, plot, summarize, or audit.
#' @param bins Number of bins used by the diagnostic.
#' @param alpha Significance or tail-probability level.
#' @export
audit_sbc <- function(x, bins = 10L, alpha = 0.01) {
  d <- if (inherits(x, "eye_irt_sbc")) x$ranks else .ep07_v_as_df(x)
  .ep07_v_require(d, c("parameter", "normalized_rank"), "x")
  bins <- as.integer(bins)
  out <- lapply(split(d, d$parameter), function(z) {
    u <- z$normalized_rank[is.finite(z$normalized_rank)]
    br <- seq(0, 1, length.out = bins + 1L)
    counts <- tabulate(cut(u, br, include.lowest = TRUE, labels = FALSE), nbins = bins)
    tst <- suppressWarnings(stats::chisq.test(counts, p = rep(1 / bins, bins)))
    data.frame(
      parameter = z$parameter[1L],
      n = length(u),
      mean_rank = mean(u),
      rank_variance = stats::var(u),
      expected_mean = 0.5,
      expected_variance = 1 / 12,
      chisq = unname(tst$statistic),
      df = unname(tst$parameter),
      p_value = tst$p.value,
      alpha = alpha,
      pass_screen = is.finite(tst$p.value) && tst$p.value >= alpha,
      stringsAsFactors = FALSE
    )
  })
  ans <- .ep07_v_bind(out)
  attr(ans, "bins") <- bins
  structure(ans, class = c("eye_sbc_audit", "data.frame"))
}

# Posterior predictive checks --------------------------------------------------

#' Posterior predictive discrepancy table
#'
#' @param observed Observed vector/data object.
#' @param replicated List of replicated datasets, or matrix with one replicate
#'   per row.
#' @param discrepancies Named list of functions mapping a dataset to one number.
#' @export
posterior_predictive_discrepancies <- function(
    observed,
    replicated,
    discrepancies = list(
      mean = function(x) mean(x, na.rm = TRUE),
      sd = function(x) stats::sd(x, na.rm = TRUE),
      zero_rate = function(x) mean(x == 0, na.rm = TRUE)
    )) {
  reps <- if (is.matrix(replicated) || is.data.frame(replicated)) {
    lapply(seq_len(nrow(replicated)), function(i) replicated[i, ])
  } else replicated
  if (!is.list(reps) || !length(reps)) stop("replicated must contain datasets.", call. = FALSE)
  if (is.null(names(discrepancies))) names(discrepancies) <- paste0("T", seq_along(discrepancies))

  out <- lapply(names(discrepancies), function(nm) {
    f <- discrepancies[[nm]]
    obs <- f(observed)
    rv <- vapply(reps, f, numeric(1))
    data.frame(discrepancy = nm, observed = obs,
               replicated_mean = mean(rv, na.rm = TRUE),
               replicated_sd = stats::sd(rv, na.rm = TRUE),
               p_lower = mean(rv <= obs, na.rm = TRUE),
               p_upper = mean(rv >= obs, na.rm = TRUE),
               p_two_sided = min(1, 2 * min(mean(rv <= obs, na.rm = TRUE),
                                             mean(rv >= obs, na.rm = TRUE))),
               stringsAsFactors = FALSE)
  })
  structure(.ep07_v_bind(out), class = c("eye_irt_ppc", "data.frame"))
}

# Stress testing ---------------------------------------------------------------

#' Run a generic misspecification stress-test grid
#'
#' @param scenarios Data frame or named list describing scenarios.
#' @param runner Function `runner(scenario, replicate)` returning a one-row or
#'   tidy data frame. Errors are retained as classified failures.
#' @param replications Number of simulation or validation replications.
#' @param seed Random-number seed.
#' @export
stress_test_misspecification <- function(scenarios, runner, replications = 50L,
                                         seed = 20260808L) {
  if (!is.function(runner)) stop("runner must be a function.", call. = FALSE)
  s <- if (is.data.frame(scenarios)) scenarios else {
    if (is.list(scenarios) && !is.null(names(scenarios))) {
      data.frame(scenario = names(scenarios), value = I(unname(scenarios)),
                 stringsAsFactors = FALSE)
    } else stop("scenarios must be a data frame or named list.", call. = FALSE)
  }
  if (!"scenario" %in% names(s)) s$scenario <- paste0("scenario_", seq_len(nrow(s)))
  set.seed(seed)
  rows <- list(); k <- 0L
  for (i in seq_len(nrow(s))) for (r in seq_len(replications)) {
    k <- k + 1L
    rows[[k]] <- tryCatch({
      z <- runner(s[i, , drop = FALSE], r)
      z <- .ep07_v_as_df(z)
      z$scenario <- s$scenario[i]
      z$replicate <- r
      z$failed <- FALSE
      z
    }, error = function(e) {
      cbind(data.frame(scenario = s$scenario[i], replicate = r,
                       failed = TRUE, stringsAsFactors = FALSE),
            validation_failure_taxonomy(e))
    })
  }
  ans <- .ep07_v_bind(rows)
  attr(ans, "seed") <- seed
  attr(ans, "replications") <- replications
  structure(ans, class = c("eye_irt_stress_test", "data.frame"))
}

#' Stress test latent distribution
#' @param runner Function that executes one stress-test scenario.
#' @param replications Number of simulation or validation replications.
#' @param seed Random-number seed.
#' @export
stress_test_latent_distribution <- function(runner, replications = 50L,
                                            seed = 20260808L) {
  scenarios <- data.frame(
    scenario = c("normal", "skewed", "student_t", "mixture_normal", "bimodal", "heavy_tail"),
    distribution = c("normal", "skewed", "student_t", "mixture_normal", "bimodal", "heavy_tail"),
    stringsAsFactors = FALSE)
  stress_test_misspecification(scenarios, runner, replications, seed)
}

#' Stress test local dependence
#' @param runner Function that executes one stress-test scenario.
#' @param strengths Local-dependence strengths to evaluate.
#' @param replications Number of simulation or validation replications.
#' @param seed Random-number seed.
#' @export
stress_test_local_dependence <- function(runner, strengths = c(0, 0.2, 0.5, 0.8),
                                         replications = 50L, seed = 20260808L) {
  scenarios <- data.frame(scenario = paste0("local_dependence_", strengths),
                          strength = strengths)
  stress_test_misspecification(scenarios, runner, replications, seed)
}

#' Stress test speededness
#' @param runner Function that executes one stress-test scenario.
#' @param proportions Speededness proportions to evaluate.
#' @param replications Number of simulation or validation replications.
#' @param seed Random-number seed.
#' @export
stress_test_speededness <- function(runner, proportions = c(0, .10, .25, .40),
                                    replications = 50L, seed = 20260808L) {
  scenarios <- data.frame(scenario = paste0("speeded_", proportions),
                          speeded_proportion = proportions)
  stress_test_misspecification(scenarios, runner, replications, seed)
}

#' Stress test missingness
#' @param runner Function that executes one stress-test scenario.
#' @param mechanisms Missingness mechanisms to evaluate.
#' @param rates Missingness rates to evaluate.
#' @param replications Number of simulation or validation replications.
#' @param seed Random-number seed.
#' @export
stress_test_missingness <- function(runner,
                                    mechanisms = c("MCAR", "MAR", "MNAR_omission", "not_reached"),
                                    rates = c(.05, .15, .30),
                                    replications = 50L, seed = 20260808L) {
  scenarios <- expand.grid(mechanism = mechanisms, rate = rates,
                           stringsAsFactors = FALSE)
  scenarios$scenario <- paste(scenarios$mechanism, scenarios$rate, sep = "_")
  stress_test_misspecification(scenarios, runner, replications, seed)
}

#' Stress test preprocessing
#' @param runner Function that executes one stress-test scenario.
#' @param variants Preprocessing variants to evaluate.
#' @param replications Number of simulation or validation replications.
#' @param seed Random-number seed.
#' @export
stress_test_preprocessing <- function(runner, variants,
                                      replications = 25L, seed = 20260808L) {
  if (is.character(variants)) {
    scenarios <- data.frame(scenario = variants, preprocessing = variants,
                            stringsAsFactors = FALSE)
  } else {
    scenarios <- .ep07_v_as_df(variants)
    if (!"scenario" %in% names(scenarios))
      scenarios$scenario <- paste0("preprocess_", seq_len(nrow(scenarios)))
  }
  stress_test_misspecification(scenarios, runner, replications, seed)
}

# External/grouped validation --------------------------------------------------

.ep07_leave_group_out <- function(data, group, fitter, predictor, scorer) {
  d <- .ep07_v_as_df(data)
  .ep07_v_require(d, group, "data")
  stopifnot(is.function(fitter), is.function(predictor), is.function(scorer))
  g <- unique(d[[group]])
  rows <- lapply(g, function(level) {
    test <- d[!is.na(d[[group]]) & d[[group]] == level, , drop = FALSE]
    train <- d[is.na(d[[group]]) | d[[group]] != level, , drop = FALSE]
    tryCatch({
      fit <- fitter(train)
      pred <- predictor(fit, test)
      sc <- scorer(test, pred)
      sc <- .ep07_v_as_df(sc)
      sc[[group]] <- level
      sc$n_train <- nrow(train); sc$n_test <- nrow(test); sc$failed <- FALSE
      sc
    }, error = function(e) {
      cbind(data.frame(n_train = nrow(train), n_test = nrow(test), failed = TRUE,
                       stringsAsFactors = FALSE),
            setNames(data.frame(level, stringsAsFactors = FALSE), group),
            validation_failure_taxonomy(e))
    })
  })
  .ep07_v_bind(rows)
}

#' External validation on a completely held-out dataset
#' @param train_data Value supplied to `train_data`; see Details for its model-specific role.
#' @param external_data Value supplied to `external_data`; see Details for its model-specific role.
#' @param fitter Model-fitting function.
#' @param predictor Prediction function.
#' @param scorer Function that scores predictions.
#' @param label Value supplied to `label`; see Details for its model-specific role.
#' @export
external_validate_irt <- function(train_data, external_data, fitter, predictor, scorer,
                                  label = "external") {
  stopifnot(is.function(fitter), is.function(predictor), is.function(scorer))
  tryCatch({
    fit <- fitter(train_data)
    pred <- predictor(fit, external_data)
    sc <- .ep07_v_as_df(scorer(external_data, pred))
    sc$validation_set <- label
    sc$n_train <- nrow(train_data); sc$n_test <- nrow(external_data); sc$failed <- FALSE
    structure(sc, class = c("eye_external_irt_validation", "data.frame"))
  }, error = function(e) {
    out <- cbind(data.frame(validation_set = label, n_train = nrow(train_data),
                            n_test = nrow(external_data), failed = TRUE),
                 validation_failure_taxonomy(e))
    structure(out, class = c("eye_external_irt_validation", "data.frame"))
  })
}

#' Leave device out validation
#' @param data Input data frame or compatible tabular object.
#' @param device Device identifier or device facet.
#' @param fitter Model-fitting function.
#' @param predictor Prediction function.
#' @param scorer Function that scores predictions.
#' @export
leave_device_out_validation <- function(data, device, fitter, predictor, scorer) {
  structure(.ep07_leave_group_out(data, device, fitter, predictor, scorer),
            class = c("eye_leave_device_out_validation", "data.frame"))
}

#' Leave session out validation
#' @param data Input data frame or compatible tabular object.
#' @param session Session identifier or session facet.
#' @param fitter Model-fitting function.
#' @param predictor Prediction function.
#' @param scorer Function that scores predictions.
#' @export
leave_session_out_validation <- function(data, session, fitter, predictor, scorer) {
  structure(.ep07_leave_group_out(data, session, fitter, predictor, scorer),
            class = c("eye_leave_session_out_validation", "data.frame"))
}

#' Leave site out validation
#' @param data Input data frame or compatible tabular object.
#' @param site Site identifier or site facet.
#' @param fitter Model-fitting function.
#' @param predictor Prediction function.
#' @param scorer Function that scores predictions.
#' @export
leave_site_out_validation <- function(data, site, fitter, predictor, scorer) {
  structure(.ep07_leave_group_out(data, site, fitter, predictor, scorer),
            class = c("eye_leave_site_out_validation", "data.frame"))
}

#' Leave item out validation
#' @param data Input data frame or compatible tabular object.
#' @param item Item identifier, name, or item column.
#' @param fitter Model-fitting function.
#' @param predictor Prediction function.
#' @param scorer Function that scores predictions.
#' @export
leave_item_out_validation <- function(data, item, fitter, predictor, scorer) {
  structure(.ep07_leave_group_out(data, item, fitter, predictor, scorer),
            class = c("eye_leave_item_out_validation", "data.frame"))
}

#' Summarise measurement transportability across held-out groups
#' @param validation Validation results or validation specification.
#' @param metric Metric to calculate or audit.
#' @param higher_is_better Whether larger metric values indicate better performance.
#' @param max_range Maximum allowed range across held-out groups.
#' @param minimum Minimum acceptable value or threshold.
#' @param maximum Maximum acceptable value or threshold.
#' @export
audit_measurement_transportability <- function(validation, metric, higher_is_better = TRUE,
                                               max_range = NULL,
                                               minimum = NULL,
                                               maximum = NULL) {
  d <- .ep07_v_as_df(validation)
  .ep07_v_require(d, metric, "validation")
  x <- .ep07_v_num(d[[metric]])
  finite <- x[is.finite(x)]
  rng <- if (length(finite)) diff(range(finite)) else NA_real_
  mean_metric <- if (length(finite)) mean(finite) else NA_real_
  pass <- TRUE
  if (!is.null(max_range)) pass <- pass && is.finite(rng) && rng <= max_range
  if (!is.null(minimum)) pass <- pass && is.finite(mean_metric) && mean_metric >= minimum
  if (!is.null(maximum)) pass <- pass && is.finite(mean_metric) && mean_metric <= maximum
  structure(data.frame(
    metric = metric,
    n_groups = length(finite),
    mean = mean_metric,
    sd = if (length(finite) > 1L) stats::sd(finite) else NA_real_,
    min = if (length(finite)) min(finite) else NA_real_,
    max = if (length(finite)) max(finite) else NA_real_,
    range = rng,
    higher_is_better = higher_is_better,
    max_allowed_range = if (is.null(max_range)) NA_real_ else max_range,
    pass = pass,
    stringsAsFactors = FALSE),
    class = c("eye_measurement_transportability_audit", "data.frame"))
}

#' Compare validation engines on common recovery output
#' @param results Validation or model results.
#' @export
compare_validation_engines <- function(results) {
  s <- summarize_parameter_recovery(results, by = c("engine", "parameter"))
  ord <- order(s$parameter, s$rmse, s$absolute_bias, -s$coverage, na.last = TRUE)
  s <- s[ord, , drop = FALSE]
  s$rank_within_parameter <- ave(seq_len(nrow(s)), s$parameter,
                                 FUN = function(ix) seq_along(ix))
  structure(s, class = c("eye_validation_engine_comparison", "data.frame"))
}

# Incremental information / negative controls ---------------------------------

#' Audit out-of-sample incremental information from a process channel
#'
#' @param data Input data.
#' @param fold Group/fold column. Every unique value is held out once.
#' @param baseline_fitter Function fitted without the process channel.
#' @param process_fitter Function fitted with the process channel.
#' @param predictor Function `(fit, test)` returning predictions.
#' @param scorer Function `(test, prediction)` returning a scalar score.
#' @param higher_is_better Direction of the score.
#' @export
audit_channel_incremental_information <- function(
    data, fold, baseline_fitter, process_fitter, predictor, scorer,
    higher_is_better = TRUE) {
  d <- .ep07_v_as_df(data); .ep07_v_require(d, fold, "data")
  funcs <- list(baseline_fitter, process_fitter, predictor, scorer)
  if (!all(vapply(funcs, is.function, logical(1))))
    stop("All fitter/predictor/scorer arguments must be functions.", call. = FALSE)
  levels <- unique(d[[fold]])
  rows <- lapply(levels, function(g) {
    test <- d[d[[fold]] == g, , drop = FALSE]
    train <- d[d[[fold]] != g, , drop = FALSE]
    tryCatch({
      b <- baseline_fitter(train); p <- process_fitter(train)
      sb <- as.numeric(scorer(test, predictor(b, test)))[1L]
      sp <- as.numeric(scorer(test, predictor(p, test)))[1L]
      imp <- if (higher_is_better) sp - sb else sb - sp
      data.frame(fold = as.character(g), baseline = sb, process = sp,
                 improvement = imp, n_test = nrow(test), failed = FALSE)
    }, error = function(e) {
      cbind(data.frame(fold = as.character(g), baseline = NA_real_, process = NA_real_,
                       improvement = NA_real_, n_test = nrow(test), failed = TRUE),
            validation_failure_taxonomy(e))
    })
  })
  out <- .ep07_v_bind(rows)
  class(out) <- c("eye_incremental_information_audit", "data.frame")
  attr(out, "mean_improvement") <- mean(out$improvement, na.rm = TRUE)
  attr(out, "positive_fold_fraction") <- mean(out$improvement > 0, na.rm = TRUE)
  out
}

#' Negative-control test for an allegedly informative process channel
#'
#' The user supplies a complete evaluation callback. eyeprocess permutes the
#' named process columns, optionally within grouping strata, and compares the
#' observed score with the permutation distribution.
#'
#' @param data Input data.
#' @param process_columns Columns to permute.
#' @param evaluator Function returning one scalar out-of-sample score.
#' @param within Optional grouping columns within which permutation occurs.
#' @param higher_is_better Score direction.
#' @param permutations Number of negative-control permutations.
#' @param seed Random-number seed.
#' @export
negative_control_process_test <- function(
    data, process_columns, evaluator, within = NULL, permutations = 100L,
    higher_is_better = TRUE, seed = 20260808L) {
  d <- .ep07_v_as_df(data)
  .ep07_v_require(d, process_columns, "data")
  if (!is.function(evaluator)) stop("evaluator must be a function.", call. = FALSE)
  obs <- as.numeric(evaluator(d))[1L]
  set.seed(seed)
  null <- numeric(permutations)
  strata <- if (is.null(within) || !length(within)) rep("all", nrow(d)) else {
    .ep07_v_require(d, within, "data")
    .ep07_v_group_key(d, within)
  }
  idx_split <- split(seq_len(nrow(d)), strata)
  for (b in seq_len(permutations)) {
    z <- d
    for (nm in process_columns) {
      for (idx in idx_split) z[idx, nm] <- sample(z[idx, nm], length(idx), replace = FALSE)
    }
    null[b] <- as.numeric(evaluator(z))[1L]
  }
  p <- if (higher_is_better) (1 + sum(null >= obs, na.rm = TRUE)) / (permutations + 1) else
    (1 + sum(null <= obs, na.rm = TRUE)) / (permutations + 1)
  structure(list(observed = obs, null = null, p_value = p,
                 permutations = permutations, process_columns = process_columns,
                 within = within, higher_is_better = higher_is_better, seed = seed),
            class = "eye_process_negative_control")
}

#' Audit transfer of calibration across devices/sessions/sites
#'
#' @param data Data containing group, observed and predicted values.
#' @param group Grouping column.
#' @param observed Observed binary/numeric outcome column.
#' @param predicted Predicted probability/numeric score column.
#' @export
calibration_transfer_audit <- function(data, group, observed, predicted) {
  d <- .ep07_v_as_df(data)
  .ep07_v_require(d, c(group, observed, predicted), "data")
  sp <- split(d, d[[group]])
  rows <- lapply(names(sp), function(g) {
    z <- sp[[g]]
    y <- .ep07_v_num(z[[observed]]); p <- .ep07_v_num(z[[predicted]])
    ok <- is.finite(y) & is.finite(p)
    y <- y[ok]; p <- p[ok]
    if (length(y) < 3L) return(data.frame(group = g, n = length(y), intercept = NA,
                                          slope = NA, brier = NA, stringsAsFactors = FALSE))
    # Logistic calibration when probabilities + binary outcomes are available;
    # otherwise use linear observed-on-predicted calibration.
    if (all(y %in% c(0, 1)) && all(p > 0 & p < 1)) {
      lp <- stats::qlogis(p)
      fit <- suppressWarnings(stats::glm(y ~ lp, family = stats::binomial()))
      co <- stats::coef(fit)
      inter <- unname(co[1]); slope <- unname(co[2]); brier <- mean((p - y)^2)
    } else {
      fit <- stats::lm(y ~ p)
      co <- stats::coef(fit)
      inter <- unname(co[1]); slope <- unname(co[2]); brier <- mean((p - y)^2)
    }
    data.frame(group = g, n = length(y), intercept = inter, slope = slope,
               brier = brier, stringsAsFactors = FALSE)
  })
  out <- .ep07_v_bind(rows)
  names(out)[names(out) == "group"] <- group
  structure(out, class = c("eye_calibration_transfer_audit", "data.frame"))
}

# Evidence grading -------------------------------------------------------------

#' Grade model evidence against an explicit validation contract
#'
#' This is intentionally conservative: failure of any required criterion caps
#' the evidence grade. It does not convert exploratory evidence into a claim of
#' substantive validity.
#' @param recovery Value supplied to `recovery`; see Details for its model-specific role.
#' @param spec IRT model or validation specification.
#' @param external_validation Value supplied to `external_validation`; see Details for its model-specific role.
#' @param sbc Value supplied to `sbc`; see Details for its model-specific role.
#' @param ppc Value supplied to `ppc`; see Details for its model-specific role.
#' @param semantic_roundtrip Value supplied to `semantic_roundtrip`; see Details for its model-specific role.
#' @export
grade_model_evidence <- function(
    recovery,
    spec = irt_validation_spec("unspecified"),
    external_validation = NULL,
    sbc = NULL,
    ppc = NULL,
    semantic_roundtrip = NULL) {
  if (!inherits(spec, "eye_irt_validation_spec"))
    stop("spec must be an eye_irt_validation_spec.", call. = FALSE)
  s <- if (inherits(recovery, "eye_irt_recovery_summary")) recovery else
    summarize_parameter_recovery(recovery)
  th <- spec$thresholds
  checks <- data.frame(
    criterion = c("absolute_bias", "rmse", "coverage", "failure_rate"),
    pass = c(
      all(s$absolute_bias <= (th$max_abs_bias %||% 0.10), na.rm = TRUE),
      all(s$rmse <= (th$max_rmse %||% 0.30), na.rm = TRUE),
      all(s$coverage >= (th$min_coverage %||% 0.90) | is.na(s$coverage), na.rm = TRUE),
      all(s$failure_rate <= (th$max_failure_rate %||% 0.05), na.rm = TRUE)
    ), stringsAsFactors = FALSE)

  if (!is.null(sbc)) {
    sa <- if (inherits(sbc, "eye_sbc_audit")) sbc else audit_sbc(sbc)
    checks <- rbind(checks, data.frame(criterion = "sbc_screen",
                                      pass = all(sa$pass_screen, na.rm = TRUE)))
  }
  if (!is.null(ppc)) {
    pd <- .ep07_v_as_df(ppc)
    if ("p_two_sided" %in% names(pd))
      checks <- rbind(checks, data.frame(criterion = "ppc_extremes",
                                        pass = all(pd$p_two_sided >= 0.01, na.rm = TRUE)))
  }
  if (!is.null(external_validation)) {
    ed <- .ep07_v_as_df(external_validation)
    nf <- if ("failed" %in% names(ed)) sum(!ed$failed, na.rm = TRUE) else nrow(ed)
    checks <- rbind(checks, data.frame(
      criterion = "external_folds",
      pass = nf >= (th$min_external_folds %||% 2L)))
  }
  if (!is.null(semantic_roundtrip)) {
    rd <- if (is.data.frame(semantic_roundtrip)) semantic_roundtrip else
      semantic_roundtrip$field_fidelity %||% data.frame()
    roundtrip_pass <- nrow(rd) > 0L &&
      !any(toupper(rd$status %||% "") %in% c("UNSUPPORTED", "AMBIGUOUS"))
    checks <- rbind(checks, data.frame(criterion = "semantic_roundtrip",
                                      pass = roundtrip_pass))
  }
  n_pass <- sum(checks$pass, na.rm = TRUE); n <- nrow(checks)
  grade <- if (n_pass == n && n >= 6L) "strong_validation_evidence" else
    if (n_pass == n) "moderate_validation_evidence" else
      if (n_pass >= ceiling(n * .75)) "provisional_validation_evidence" else
        "insufficient_validation_evidence"
  structure(list(model_id = spec$model_id, grade = grade, checks = checks,
                 recovery = s, contract = spec,
                 warning = paste(
                   "Evidence grades summarize the supplied validation programme.",
                   "They are not a substitute for substantive validity, independent",
                   "replication, or model-specific scientific judgment.")),
            class = "eye_irt_evidence_grade")
}

#' Print eye irt validation spec
#' @param x Object to print, plot, summarize, or audit.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @export
print.eye_irt_validation_spec <- function(x, ...) {
  cat("eyeprocess IRT validation specification\n")
  cat("  model:", x$model_id, "\n")
  cat("  replications:", x$replications, "\n")
  cat("  metrics:", paste(x$metrics, collapse = ", "), "\n")
  if (length(x$grouped_validation))
    cat("  grouped validation:", paste(x$grouped_validation, collapse = ", "), "\n")
  invisible(x)
}

#' Print eye irt evidence grade
#' @param x Object to print, plot, summarize, or audit.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @export
print.eye_irt_evidence_grade <- function(x, ...) {
  cat("eyeprocess model-evidence grade\n")
  cat("  model:", x$model_id, "\n")
  cat("  grade:", x$grade, "\n")
  cat("  passed:", sum(x$checks$pass, na.rm = TRUE), "/", nrow(x$checks), "criteria\n")
  invisible(x)
}

#' Print eye process negative control
#' @param x Object to print, plot, summarize, or audit.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @export
print.eye_process_negative_control <- function(x, ...) {
  cat("eyeprocess process-channel negative control\n")
  cat("  observed score:", signif(x$observed, 5), "\n")
  cat("  permutation p:", signif(x$p_value, 5), "\n")
  cat("  permutations:", x$permutations, "\n")
  invisible(x)
}
